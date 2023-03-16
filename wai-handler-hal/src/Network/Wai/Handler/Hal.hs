{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
--
-- Module      : Network.Wai.Handler.Hal
-- Copyright   : (C) 2021 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- Lifts an 'Wai.Application' so that it can be run using
-- 'AWS.Lambda.Runtime.mRuntime' or
-- 'AWS.Lambda.Runtime.mRuntimeWithContext''. The glue code will look
-- something like this:
--
-- @
-- import AWS.Lambda.Runtime ('AWS.Lambda.Runtime.mRuntime')
-- import Network.Wai (Application)
-- import qualified Network.Wai.Handler.Hal as WaiHandler
--
-- app :: Application
-- app = undefined -- From Servant or wherever else
--
-- main :: IO ()
-- main = 'AWS.Lambda.Runtime.mRuntime' $ WaiHandler.'run' app
-- @
module Network.Wai.Handler.Hal
  ( run,
    runWithContext,
    toWaiRequest,
    fromWaiResponse,
  )
where

import AWS.Lambda.Context (LambdaContext)
import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest as HalRequest
  ( RequestContext (identity),
  )
import qualified AWS.Lambda.Events.ApiGateway.ProxyRequest as HalRequest hiding
  ( RequestContext (..),
  )
import qualified AWS.Lambda.Events.ApiGateway.ProxyResponse as HalResponse
import Control.Exception (IOException, tryJust)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Function ((&))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as H
import qualified Data.IORef as IORef
import Data.List (foldl', sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vault.Lazy (Key, Vault)
import qualified Data.Vault.Lazy as Vault
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
import System.IO
  ( IOMode (..),
    SeekMode (..),
    hPutStrLn,
    hSeek,
    stderr,
    withFile,
  )

-- | Convert a WAI 'Wai.Application' into a function that can
-- be run by hal's 'AWS.Lambda.Runtime.mRuntime'. This is the simplest
-- form, and probably all that you'll need. See 'runWithContext' if
-- you have more complex needs.
run ::
  MonadIO m =>
  Wai.Application ->
  HalRequest.ProxyRequest HalRequest.NoAuthorizer ->
  m HalResponse.ProxyResponse
run app req = liftIO $ do
  waiReq <- toWaiRequest Vault.empty 443 req
  responseRef <- IORef.newIORef Nothing
  Wai.ResponseReceived <- app waiReq $ \waiResp ->
    Wai.ResponseReceived <$ IORef.writeIORef responseRef (Just waiResp)
  Just waiResp <- IORef.readIORef responseRef
  fromWaiResponse waiResp

-- | Convert a WAI 'Wai.Application' into a function that can
-- be run by hal's 'AWS.Lambda.Runtime.mRuntimeWithContext''. This
-- function exposes all the configurable knobs.
runWithContext ::
  MonadIO m =>
  -- | Vault of values to share between the application and any
  -- middleware. You can pass in @Data.Vault.Lazy.'Vault.empty'@, or
  -- 'mempty' if you don't want to depend on @vault@ directly.
  Vault ->
  -- | API Gateway doesn't tell us the port it's listening on, so you
  -- have to tell it yourself. This is almost always going to be 443
  -- (HTTPS).
  PortNumber ->
  -- | We pass two 'Vault' keys to the callback that provides the
  -- 'Wai.Application'. This allows the application to look into the
  -- 'Vault' part of each request and read @hal@ data structures, if
  -- necessary:
  --
  -- * The @'Key' 'LambdaContext'@ provides
  --   information about the Lambda invocation, function, and
  --   execution environment; and
  --
  -- * The @'Key' ('HalRequest.ProxyRequest'
  -- 'HalRequest.NoAuthorizer')@ provides the unmodified API Gateway
  -- representation of the HTTP request.
  ( Key LambdaContext ->
    Key (HalRequest.ProxyRequest HalRequest.NoAuthorizer) ->
    Wai.Application
  ) ->
  LambdaContext ->
  -- | We force 'HalRequest.NoAuthorizer' because it's a type alias
  -- for 'Data.Aeson.Value' (i.e., should always parse), and it avoids
  -- an "ambiguous type variable" error at the use site.
  HalRequest.ProxyRequest HalRequest.NoAuthorizer ->
  m HalResponse.ProxyResponse
runWithContext vault port app ctx req = liftIO $ do
  contextKey <- Vault.newKey
  requestKey <- Vault.newKey
  let vault' =
        vault
          & Vault.insert contextKey ctx
          & Vault.insert requestKey req
  waiReq <- toWaiRequest vault' port req
  responseRef <- IORef.newIORef Nothing
  Wai.ResponseReceived <- app contextKey requestKey waiReq $ \waiResp ->
    Wai.ResponseReceived <$ IORef.writeIORef responseRef (Just waiResp)
  Just waiResp <- IORef.readIORef responseRef
  fromWaiResponse waiResp

-- | Convert the request sent to a Lambda serving an API Gateway proxy
-- integration into a WAI request.
--
-- __Note:__ We aren't told the HTTP version the client is using, so
-- we assume HTTP 1.1.
toWaiRequest ::
  Vault ->
  PortNumber ->
  HalRequest.ProxyRequest a ->
  IO Wai.Request
toWaiRequest vault port req = do
  let pathSegments = T.splitOn "/" . T.dropWhile (== '/') $ HalRequest.path req
      query = sort . constructQuery $ HalRequest.multiValueQueryStringParameters req
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
  -- Test invokes from the API Gateway console pass a "sourceIp" field
  -- of "test-invoke-source-ip". If the getAddrInfo call fails, just
  -- assume localhost.
  sourceHost <-
    tryJust
      (Just @IOException)
      (NS.getAddrInfo (Just hints) (Just sourceIp) (Just $ show port))
      >>= \case
        Right (s : _) -> pure $ NS.addrAddress s
        _ -> do
          hPutStrLn stderr $
            mconcat
              [ "Cannot convert sourceIp ",
                show sourceIp,
                " to address; assuming 127.0.0.1"
              ]
          pure . NS.SockAddrInet port $ NS.tupleToHostAddress (127, 0, 0, 1)
  body <- returnChunks $ HalRequest.body req
  let waiReq =
        Wai.Request
          { Wai.requestMethod = T.encodeUtf8 $ HalRequest.httpMethod req,
            Wai.httpVersion = HttpVersion 1 1,
            Wai.rawPathInfo =
              BL.toStrict
                . Builder.toLazyByteString
                $ encodePath pathSegments [],
            Wai.rawQueryString = case query of
              [] -> ""
              _ -> renderQuery True query,
            Wai.requestHeaders =
              sort
                . foldMap
                  ( \(hName, hValues) ->
                      (CI.map T.encodeUtf8 hName,) . T.encodeUtf8 <$> hValues
                  )
                . H.toList
                $ HalRequest.multiValueHeaders req,
            Wai.isSecure = True,
            Wai.remoteHost = sourceHost,
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

-- | Convert a WAI 'Wai.Response' into a hal
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
