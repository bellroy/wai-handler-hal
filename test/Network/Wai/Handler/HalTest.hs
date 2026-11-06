{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Network.Wai.Handler.HalTest where

import AWS.Lambda.Events.ApiGateway.ProxyRequest (ProxyRequest)
import AWS.Lambda.Events.ApiGateway.ProxyResponse
  ( ProxyBody (..),
    ProxyResponse (..),
  )
import Data.Aeson (eitherDecodeFileStrict')
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Void (Void)
import Network.HTTP.Types (hContentEncoding, hContentType, ok200)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Hal
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Text.Pretty.Simple (pShowNoColor)

test_ConvertProxyRequest :: TestTree
test_ConvertProxyRequest =
  goldenVsString "API Gateway Proxy Request" "test/golden/WaiRequest.txt" $ do
    proxyRequest :: ProxyRequest Void <-
      eitherDecodeFileStrict' "test/data/ProxyRequest.json"
        >>= either fail pure
    waiRequest <- toWaiRequest defaultOptions proxyRequest
    pure . TL.encodeUtf8 $ pShowNoColor waiRequest

unit_BinaryResponse :: Assertion
unit_BinaryResponse = do
  let options = defaultOptions {binaryMediaTypes = ["*/*"]}
  ProxyResponse {body = ProxyBody {..}} <-
    fromWaiResponse options helloWorld

  assertEqual "response is binary" True isBase64Encoded
  assertEqual
    "response is base64-encoded"
    (Right "Hello, World!")
    (B64.decode (T.encodeUtf8 serialized))

unit_TextResponse :: Assertion
unit_TextResponse = do
  ProxyResponse {body = ProxyBody {..}} <-
    fromWaiResponse defaultOptions helloWorld

  assertEqual "response is not binary" False isBase64Encoded
  assertEqual "response is unmangled" "Hello, World!" serialized

helloWorld :: Response
helloWorld = responseLBS ok200 [(hContentType, "text/plain")] "Hello, World!"

unit_GzipResponseIsBase64Encoded :: Assertion
unit_GzipResponseIsBase64Encoded = do
  let gzipBytes = "\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\x03\xff\xfe"
  ProxyResponse {body = ProxyBody {..}} <-
    fromWaiResponse defaultOptions $
      responseLBS
        ok200
        [(hContentType, "text/html"), (hContentEncoding, "gzip")]
        (LBS.fromStrict gzipBytes)

  assertEqual "response is binary" True isBase64Encoded
  assertEqual
    "response round-trips through base64"
    (Right gzipBytes)
    (B64.decode (T.encodeUtf8 serialized))

unit_IdentityResponseIsNotBase64Encoded :: Assertion
unit_IdentityResponseIsNotBase64Encoded = do
  ProxyResponse {body = ProxyBody {..}} <-
    fromWaiResponse defaultOptions $
      responseLBS
        ok200
        [(hContentType, "text/plain"), (hContentEncoding, "identity")]
        "Hello, World!"

  assertEqual "response is not binary" False isBase64Encoded
  assertEqual "response is unmangled" "Hello, World!" serialized

test_ContentEncodingDetection :: [TestTree]
test_ContentEncodingDetection =
  [ testGroup "base64-encoded" $
      [ ("Gzip", ["Gzip"]),
        ("br, gzip with whitespace", [" br , gzip"]),
        ("identity, gzip", ["identity, gzip"]),
        ("x-gzip", ["x-gzip"]),
        ("repeated headers", ["identity", "br"])
      ]
        <&> \(title, values) -> testCase title $ do
          ProxyResponse {body = ProxyBody {..}} <- mkResponse values
          assertEqual "isBase64Encoded" True isBase64Encoded,
    testGroup "not base64-encoded" $
      [ ("empty value", [""]),
        ("identity, mixed case", ["identity , Identity"]),
        ("no header", [])
      ]
        <&> \(title, values) -> testCase title $ do
          ProxyResponse {body = ProxyBody {..}} <- mkResponse values
          assertEqual "isBase64Encoded" False isBase64Encoded
  ]
  where
    mkResponse :: [ByteString] -> IO ProxyResponse
    mkResponse values =
      fromWaiResponse defaultOptions $
        responseLBS
          ok200
          ((hContentType, "text/plain") : ((hContentEncoding,) <$> values))
          "x"
