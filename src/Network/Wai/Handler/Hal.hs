{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
--
-- Module      : Network.Wai.Handler.Hal
-- Copyright   : (C) 2021, 2024 Bellroy Pty Ltd
-- License     : BSD-3-Clause
-- Maintainer  : Bellroy Tech Team <haskell@bellroy.com>
-- Stability   : experimental
--
-- Lifts an 'Wai.Application' so that it can be run using
-- 'AWS.Lambda.Runtime.mRuntime' or
-- 'AWS.Lambda.Runtime.mRuntimeWithContext'. The glue code will look
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
    runWithOptions,
    runWithContext,
    Options (..),
    defaultOptions,
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
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.CaseInsensitive as CI
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IORef as IORef
import Data.List (foldl', sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vault.Lazy (Key, Vault)
import qualified Data.Vault.Lazy as Vault
import Network.HTTP.Media (MediaType, matches, parseAccept, renderHeader)
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
  (MonadIO m) =>
  Wai.Application ->
  HalRequest.ProxyRequest HalRequest.NoAuthorizer ->
  m HalResponse.ProxyResponse
run = runWithOptions defaultOptions

-- | Options that can be used to customize the behaviour of 'runWithContext'.
-- 'defaultOptions' provides sensible defaults.
data Options = Options
  { -- | Vault of values to share between the application and any
    -- middleware. You can pass in @Data.Vault.Lazy.'Vault.empty'@, or
    -- 'mempty' if you don't want to depend on @vault@ directly.
    vault :: Vault,
    -- | API Gateway doesn't tell us the port it's listening on, so you
    -- have to tell it yourself. This is almost always going to be 443
    -- (HTTPS).
    portNumber :: PortNumber,
    -- | To return binary data, API Gateway requires you to configure
    -- the @binaryMediaTypes@ setting on your API, and then
    -- base64-encode your binary responses.
    --
    -- If the @Content-Type@ header in the @wai@ 'Wai.Response'
    -- matches any of the media types in this field, @wai-handler-hal@
    -- will base64-encode its response to the API Gateway.
    --
    -- If you set @binaryMediaTypes@ in your API, you should override
    -- the default (empty) list to match.
    --
    -- /See:/ [Content type conversion in API Gateway](https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-payload-encodings-workflow.html)
    -- in the [Amazon API Gateway Developer Guide](https://docs.aws.amazon.com/apigateway/latest/developerguide/).
    binaryMediaTypes :: [MediaType]
  }

-- | Default options for running 'Wai.Application's on Lambda.
defaultOptions :: Options
defaultOptions =
  Options
    { vault = Vault.empty,
      portNumber = 443,
      binaryMediaTypes = []
    }

-- | A variant of 'run' with configurable 'Options'. Useful if you
-- just want to override the 'binaryMediaTypes' setting but don't need
-- the rest of 'runWithContext'\'s features.
--
-- @since 0.4.0.0
runWithOptions ::
  (MonadIO m) =>
  -- | Configuration options. 'defaultOptions' provides sensible defaults.
  Options ->
  Wai.Application ->
  HalRequest.ProxyRequest HalRequest.NoAuthorizer ->
  m HalResponse.ProxyResponse
runWithOptions opts app req = liftIO $ do
  waiReq <- toWaiRequest opts req
  responseRef <- IORef.newIORef Nothing
  Wai.ResponseReceived <- app waiReq $ \waiResp ->
    Wai.ResponseReceived <$ IORef.writeIORef responseRef (Just waiResp)
  Just waiResp <- IORef.readIORef responseRef
  fromWaiResponse opts waiResp

-- | Convert a WAI 'Wai.Application' into a function that can
-- be run by hal's 'AWS.Lambda.Runtime.mRuntimeWithContext'. This
-- function exposes all the configurable knobs.
runWithContext ::
  (MonadIO m) =>
  -- | Configuration options. 'defaultOptions' provides sensible defaults.
  Options ->
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
runWithContext opts app ctx req = liftIO $ do
  contextKey <- Vault.newKey
  requestKey <- Vault.newKey
  let vault' =
        vault opts
          & Vault.insert contextKey ctx
          & Vault.insert requestKey req
      opts' = opts {vault = vault'}
  waiReq <- toWaiRequest opts' req
  responseRef <- IORef.newIORef Nothing
  Wai.ResponseReceived <- app contextKey requestKey waiReq $ \waiResp ->
    Wai.ResponseReceived <$ IORef.writeIORef responseRef (Just waiResp)
  Just waiResp <- IORef.readIORef responseRef
  fromWaiResponse opts' waiResp

-- | Convert the request sent to a Lambda serving an API Gateway proxy
-- integration into a WAI request.
--
-- When processing multiple values for the same
-- [HTTP header|query parameter], API Gateway will put the final such
-- value into ['HalRequest.headers'|'HalRequest.queryStringParameters']
-- and all such values into
-- ['HalRequest.multiValueHeaders'|'HalRequest.multiValueQueryStringParameters'].
-- We merge both but exclusively use the multi-value version if it exists,
-- to ensure that [headers|query parameters] defined in hand-crafted
-- 'HalRequest.ProxyRequest's always appear in the 'WaiRequest.Request'.
--
-- __Note:__ We aren't told the HTTP version the client is using, so
-- we assume HTTP 1.1.
toWaiRequest ::
  Options ->
  HalRequest.ProxyRequest a ->
  IO Wai.Request
toWaiRequest opts req = do
  let port = portNumber opts
      pathSegments =
        Text.splitOn "/" . Text.dropWhile (== '/') $ HalRequest.path req
      query =
        sort . constructQuery $
          HalRequest.multiValueQueryStringParameters req
            `HashMap.union` ((: []) <$> HalRequest.queryStringParameters req)
      hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_NUMERICHOST],
            NS.addrSocketType = NS.Stream
          }
      sourceIp =
        Text.unpack
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
        Right (s :| _) -> pure $ NS.addrAddress s
        _ -> do
          hPutStrLn stderr $
            mconcat
              [ "Cannot convert sourceIp ",
                show sourceIp,
                " to address; assuming 127.0.0.1"
              ]
          pure . NS.SockAddrInet port $ NS.tupleToHostAddress (127, 0, 0, 1)
  body <- returnChunks $ HalRequest.body req
  pure
    Wai.Request
      { Wai.requestMethod = Text.encodeUtf8 $ HalRequest.httpMethod req,
        Wai.httpVersion = HttpVersion 1 1,
        Wai.rawPathInfo =
          LByteString.toStrict
            . Builder.toLazyByteString
            $ encodePath pathSegments [],
        Wai.rawQueryString = case query of
          [] -> ""
          _ -> renderQuery True query,
        Wai.requestHeaders =
          sort
            . foldMap
              ( \(hName, hValues) ->
                  hValues <&> \hValue ->
                    (CI.map Text.encodeUtf8 hName, Text.encodeUtf8 hValue)
              )
            . HashMap.toList
            $ HalRequest.multiValueHeaders req
              `HashMap.union` ((: []) <$> HalRequest.headers req),
        Wai.isSecure = True,
        Wai.remoteHost = sourceHost,
        Wai.pathInfo = pathSegments,
        Wai.queryString = query,
        Wai.requestBody = body,
        Wai.vault = vault opts,
        Wai.requestBodyLength =
          Wai.KnownLength
            . fromIntegral
            . LByteString.length
            $ HalRequest.body req,
        Wai.requestHeaderHost = getHeader hHost req,
        Wai.requestHeaderRange = getHeader hRange req,
        Wai.requestHeaderReferer = getHeader hReferer req,
        Wai.requestHeaderUserAgent = getHeader hUserAgent req
      }

-- | Unpack a lazy 'LByteString.ByteString' into its chunks, and
-- return an IO action which returns each chunk in sequence, and
-- returns 'ByteString.empty' forever after the bytestring is
-- exhausted.
returnChunks :: LByteString.ByteString -> IO (IO ByteString.ByteString)
returnChunks bs = do
  chunkRef <- IORef.newIORef $ LByteString.toChunks bs
  pure . IORef.atomicModifyIORef' chunkRef $
    \case
      [] -> mempty
      (ch : chs) -> (chs, ch)

constructQuery :: HashMap Text [Text] -> Query
constructQuery = foldMap expandParamList . HashMap.toList
  where
    expandParamList :: (Text, [Text]) -> [QueryItem]
    expandParamList (param, values) =
      queryTextToQuery $ case values of
        [] -> [(param, Nothing)]
        _ -> (param,) . Just <$> values

getHeader :: HeaderName -> HalRequest.ProxyRequest a -> Maybe ByteString
getHeader h =
  fmap Text.encodeUtf8
    . HashMap.lookup (CI.map Text.decodeUtf8Lenient h)
    . HalRequest.headers

-- | Convert a WAI 'Wai.Response' into a hal
-- 'HalResponse.ProxyResponse'.
fromWaiResponse :: Options -> Wai.Response -> IO HalResponse.ProxyResponse
fromWaiResponse opts (Wai.ResponseFile status headers path mFilePart) = do
  fileData <- readFilePart path mFilePart
  pure
    . addHeaders headers
    . HalResponse.response status
    . createProxyBody opts (getContentType headers)
    $ fileData
fromWaiResponse opts (Wai.ResponseBuilder status headers builder) =
  pure
    . addHeaders headers
    . HalResponse.response status
    . createProxyBody opts (getContentType headers)
    . LByteString.toStrict
    $ Builder.toLazyByteString builder
fromWaiResponse opts (Wai.ResponseStream status headers stream) = do
  builderRef <- IORef.newIORef mempty
  let addChunk chunk = IORef.modifyIORef builderRef (<> chunk)
      flush = IORef.modifyIORef builderRef (<> Builder.flush)
  stream addChunk flush
  builder <- IORef.readIORef builderRef
  fromWaiResponse opts (Wai.ResponseBuilder status headers builder)
fromWaiResponse opts (Wai.ResponseRaw _ resp) = fromWaiResponse opts resp

readFilePart :: FilePath -> Maybe Wai.FilePart -> IO ByteString
readFilePart path mPart = withFile path ReadMode $ \h -> do
  case mPart of
    Nothing -> ByteString.hGetContents h
    Just (Wai.FilePart offset count _) -> do
      hSeek h AbsoluteSeek offset
      ByteString.hGet h $ fromIntegral count

createProxyBody :: Options -> MediaType -> ByteString -> HalResponse.ProxyBody
createProxyBody opts contentType body =
  HalResponse.ProxyBody
    { HalResponse.contentType = Text.decodeUtf8Lenient $ renderHeader contentType,
      HalResponse.serialized =
        if isBase64Encoded
          then Text.decodeUtf8Lenient $ Base64.encode body
          else Text.decodeUtf8Lenient body,
      HalResponse.isBase64Encoded
    }
  where
    isBase64Encoded = any (contentType `matches`) $ binaryMediaTypes opts

addHeaders ::
  ResponseHeaders -> HalResponse.ProxyResponse -> HalResponse.ProxyResponse
addHeaders headers response = foldl' addHeader response headers
  where
    addHeader r (hName, hValue) =
      HalResponse.addHeader
        (Text.decodeUtf8Lenient $ CI.original hName)
        (Text.decodeUtf8Lenient hValue)
        r

-- | Try to find the content-type of a response, given the response
-- headers. If we can't, return @"application/octet-stream"@.
getContentType :: ResponseHeaders -> MediaType
getContentType headers =
  fromMaybe "application/octet-stream" $
    lookup hContentType headers >>= parseAccept
