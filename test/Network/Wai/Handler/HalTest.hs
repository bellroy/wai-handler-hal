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
  assertBinary False "text/plain"
  assertBinary False "text/html"
  assertBinary False "application/json"
  assertBinary False "application/xml"
  assertBinary False "application/vnd.api+json"
  assertBinary False "application/vnd.api+xml"
  assertBinary False "image/svg+xml"

  assertBinary True "application/octet-stream"
  assertBinary True "audio/vorbis"
  assertBinary True "image/png"
  where
    assertBinary expected mime =
      assertEqual
        mime
        (binaryMimeType defaultOptions (T.pack mime))
        expected
