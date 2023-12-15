{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.HalTest where

import AWS.Lambda.Events.ApiGateway.ProxyRequest
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Void (Void)
import Network.Wai.Handler.Hal
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Pretty.Simple

test_ConvertProxyRequest :: TestTree
test_ConvertProxyRequest =
  goldenVsString "API Gateway Proxy Request" "test/golden/WaiRequest.txt" $ do
    proxyRequest :: ProxyRequest Void <-
      eitherDecodeFileStrict' "test/data/ProxyRequest.json"
        >>= either fail pure
    waiRequest <- toWaiRequest defaultOptions proxyRequest
    pure . TL.encodeUtf8 $ pShowNoColor waiRequest

test_DefaultBinaryMimeTypes :: TestTree
test_DefaultBinaryMimeTypes = testCase "default binary MIME types" $ do
  assertBinary "text/plain"               False
  assertBinary "text/html"                False
  assertBinary "application/json"         False
  assertBinary "application/xml"          False
  assertBinary "application/vnd.api+json" False
  assertBinary "application/vnd.api+xml"  False
  assertBinary "image/svg+xml"            False

  assertBinary "application/octet-stream" True
  assertBinary "audio/vorbis"             True
  assertBinary "image/png"                True
  where
    assertBinary mime expected = assertEqual
      mime
      (binaryMimeType defaultOptions (T.pack mime))
      expected
