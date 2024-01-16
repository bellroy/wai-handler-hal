{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.HalTest where

import AWS.Lambda.Events.ApiGateway.ProxyRequest (ProxyRequest)
import AWS.Lambda.Events.ApiGateway.ProxyResponse
  ( ProxyBody (..),
    ProxyResponse (..),
  )
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Void (Void)
import Network.HTTP.Types (hContentType, ok200)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Hal
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Pretty.Simple (pShowNoColor)

test_ConvertProxyRequest :: TestTree
test_ConvertProxyRequest =
  goldenVsString "API Gateway Proxy Request" "test/golden/WaiRequest.txt" $ do
    proxyRequest :: ProxyRequest Void <-
      eitherDecodeFileStrict' "test/data/ProxyRequest.json"
        >>= either fail pure
    waiRequest <- toWaiRequest defaultOptions proxyRequest
    pure . TL.encodeUtf8 $ pShowNoColor waiRequest

test_BinaryResponse :: TestTree
test_BinaryResponse = testCase "Responding to API Gateway with text" $ do
  let options = defaultOptions {binaryMediaTypes = ["*/*"]}
  ProxyResponse {body = ProxyBody {..}} <-
    fromWaiResponse options helloWorld

  assertEqual "response is binary" True isBase64Encoded
  assertEqual
    "response is base64-encoded"
    (Right "Hello, World!")
    (B64.decode (T.encodeUtf8 serialized))

test_TextResponse :: TestTree
test_TextResponse = testCase "Responding to API Gateway with text" $ do
  ProxyResponse {body = ProxyBody {..}} <-
    fromWaiResponse defaultOptions helloWorld

  assertEqual "response is not binary" False isBase64Encoded
  assertEqual "response is unmangled" "Hello, World!" serialized

helloWorld :: Response
helloWorld = responseLBS ok200 [(hContentType, "text/plain")] "Hello, World!"
