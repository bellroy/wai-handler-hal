{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
--
-- Module      : Network.Wai.Handler.Hal
-- Copyright   : (C) 2021 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- Lifts a 'Network.Wai.Application' so that it can be run by
-- 'AWS.Lambda.Runtime.mRuntimeWithContext'. The glue code will look
-- something like this:
--
-- @
-- import AWS.Lambda.Context (runReaderTLambdaContext)
-- import AWS.Lambda.Runtime (mRuntimeWithContext)
-- import qualified Data.Vault.Lazy as Vault
-- import Network.Wai (Application)
-- import qualified Network.Wai.Handler.Hal as WaiHandler
--
-- app :: Application
-- app = undefined -- From Servant or wherever else
--
-- main :: IO ()
-- main =
--   runReaderTLambdaContext . mRuntimeWithContext $
--     WaiHandler.run Vault.empty 443 app
-- @
module Network.Wai.Handler.Hal (run, toWaiRequest, fromWaiResponse) where

import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest as HalRequest
  ( RequestContext (identity),
  )
import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest as HalRequest hiding
  ( RequestContext (..),
  )
import qualified AWS.Lambda.Events.ApiGateway.ProxyResponse as HalResponse
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import qualified Data.IORef as IORef
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vault.Lazy (Vault)
import Network.HTTP.Types.Header
  ( HeaderName,
    ResponseHeaders,
    hContentType,
    hHost,
    hRange,
    hReferer,
    hUserAgent,
  )
import Network.HTTP.Types.URI
  ( Query,
    QueryItem,
    encodePath,
    queryTextToQuery,
    renderQuery,
  )
import Network.HTTP.Types.Version (HttpVersion (..))
import Network.Socket (PortNumber)
import qualified Network.Socket as NS
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import System.IO (IOMode (..), SeekMode (..), hSeek, withFile)

-- | Convert a Wai 'Wai.Application' into a function that can be run
-- by Hal's 'AWS.Lambda.Runtime.mRuntimeWithContext'.
run ::
  MonadIO m =>
  -- | Vault of values to share between the application and any
  -- middleware. You can pass in @Vault.empty@, or @mempty@ if you
  -- don't want to depend on @vault@ directly.
  Vault ->
  -- | API Gateway doesn't tell us the port it's listening on. This is
  -- almost always going to be 443 (HTTPS).
  PortNumber ->
  Wai.Application ->
  -- | We force 'HalRequest.NoAuthorizer' because WAI apps can't know
  -- about Lambda contexts, and it avoids an "ambiguous type variable"
  -- error at the use site.
  HalRequest.ProxyRequest HalRequest.NoAuthorizer ->
  m HalResponse.ProxyResponse
run vault port app req = liftIO $ do
  waiReq <- toWaiRequest vault port req
  responseRef <- IORef.newIORef Nothing
  Wai.ResponseReceived <- app waiReq $ \waiResp ->
    Wai.ResponseReceived <$ IORef.writeIORef responseRef (Just waiResp)
  Just waiResp <- IORef.readIORef responseRef
  fromWaiResponse waiResp

-- | Convert the request sent to a Lambda serving an API Gateway proxy
-- integration into a WAI request.
--
-- **Note:** We aren't told the HTTP version the client is using, so
-- we assume HTTP 1.1.
toWaiRequest ::
  Vault ->
  PortNumber ->
  HalRequest.ProxyRequest a ->
  IO Wai.Request
toWaiRequest vault port req = do
  let pathSegments = T.splitOn "/" . T.dropWhile (== '/') $ HalRequest.path req
      query = constructQuery $ HalRequest.multiValueQueryStringParameters req
      hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_NUMERICHOST],
            NS.addrFamily = NS.AF_INET,
            NS.addrSocketType = NS.Stream
          }
      sourceIp =
        T.unpack
          . HalRequest.sourceIp
          . HalRequest.identity
          $ HalRequest.requestContext req
  print $ HalRequest.path req
  print pathSegments
  sourceAddr : _ <- NS.getAddrInfo (Just hints) (Just sourceIp) (Just $ show port)
  body <- returnChunks $ HalRequest.body req
  let waiReq =
        Wai.Request
          { Wai.requestMethod = T.encodeUtf8 $ HalRequest.httpMethod req,
            Wai.httpVersion = HttpVersion 1 1,
            Wai.rawPathInfo =
              BL.toStrict
                . Builder.toLazyByteString
                $ encodePath pathSegments query,
            Wai.rawQueryString = case query of
              [] -> ""
              _ -> renderQuery True query,
            Wai.requestHeaders =
              foldMap
                ( \(hName, hValues) ->
                    (CI.map T.encodeUtf8 hName,) . T.encodeUtf8 <$> hValues
                )
                . H.toList
                $ HalRequest.multiValueHeaders req,
            Wai.isSecure = True,
            Wai.remoteHost = NS.addrAddress sourceAddr,
            Wai.pathInfo = pathSegments,
            Wai.queryString = query,
            Wai.requestBody = body,
            Wai.vault = vault,
            Wai.requestBodyLength =
              Wai.KnownLength . fromIntegral . BL.length $ HalRequest.body req,
            Wai.requestHeaderHost = getHeader hHost req,
            Wai.requestHeaderRange = getHeader hRange req,
            Wai.requestHeaderReferer = getHeader hReferer req,
            Wai.requestHeaderUserAgent = getHeader hUserAgent req
          }
  print waiReq
  pure waiReq

-- | Unpack a lazy 'BL.ByteString' into its chunks, and return an IO
-- action which returns each chunk in sequence, and returns 'B.empty'
-- forever after the bytestring is exhausted.
returnChunks :: BL.ByteString -> IO (IO B.ByteString)
returnChunks bs = do
  chunkRef <- IORef.newIORef $ BL.toChunks bs
  pure . IORef.atomicModifyIORef' chunkRef $
    \case
      [] -> mempty
      (ch : chs) -> (chs, ch)

constructQuery :: HashMap Text [Text] -> Query
constructQuery = foldMap expandParamList . H.toList
  where
    expandParamList :: (Text, [Text]) -> [QueryItem]
    expandParamList (param, values) =
      queryTextToQuery $ case values of
        [] -> [(param, Nothing)]
        _ -> (param,) . Just <$> values

getHeader :: HeaderName -> HalRequest.ProxyRequest a -> Maybe ByteString
getHeader h =
  fmap T.encodeUtf8 . H.lookup (CI.map T.decodeUtf8 h) . HalRequest.headers

-- | Convert a Wai 'Wai.Response' into a Hal
-- 'HalResponse.ProxyResponse'.
fromWaiResponse :: Wai.Response -> IO HalResponse.ProxyResponse
fromWaiResponse (Wai.ResponseFile status headers path mFilePart) = do
  fileData <- readFilePart path mFilePart
  pure
    . addHeaders headers
    . HalResponse.response status
    . createProxyBody (getContentType headers)
    $ fileData
fromWaiResponse (Wai.ResponseBuilder status headers builder) =
  pure
    . addHeaders headers
    . HalResponse.response status
    . createProxyBody (getContentType headers)
    . BL.toStrict
    $ Builder.toLazyByteString builder
fromWaiResponse (Wai.ResponseStream status headers stream) = do
  builderRef <- IORef.newIORef mempty
  let addChunk chunk = IORef.modifyIORef builderRef (<> chunk)
      flush = IORef.modifyIORef builderRef (<> Builder.flush)
  stream addChunk flush
  builder <- IORef.readIORef builderRef
  fromWaiResponse (Wai.ResponseBuilder status headers builder)
fromWaiResponse (Wai.ResponseRaw _ resp) = fromWaiResponse resp

readFilePart :: FilePath -> Maybe Wai.FilePart -> IO ByteString
readFilePart path mPart = withFile path ReadMode $ \h -> do
  case mPart of
    Nothing -> B.hGetContents h
    Just (Wai.FilePart offset count _) -> do
      hSeek h AbsoluteSeek offset
      B.hGet h $ fromIntegral count

createProxyBody :: Text -> ByteString -> HalResponse.ProxyBody
createProxyBody contentType body
  | any (`T.isPrefixOf` contentType) ["text/plain", "application/json"] =
    HalResponse.ProxyBody contentType (T.decodeUtf8 body) False
  | otherwise =
    HalResponse.ProxyBody contentType (T.decodeUtf8 $ B64.encode body) True

addHeaders ::
  ResponseHeaders -> HalResponse.ProxyResponse -> HalResponse.ProxyResponse
addHeaders headers response = foldl' addHeader response headers
  where
    addHeader r (hName, hValue) =
      HalResponse.addHeader
        (T.decodeUtf8 $ CI.original hName)
        (T.decodeUtf8 hValue)
        r

-- | Try to find the content-type of a response, given the response
-- headers. If we can't, return @"application/octet-stream"@.
getContentType :: ResponseHeaders -> Text
getContentType =
  maybe "application/octet-stream" T.decodeUtf8 . lookup hContentType
