{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.HalTest where

import AWS.Lambda.Events.ApiGateway.ProxyRequest
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.Text.Lazy.Encoding as T
import Data.Void (Void)
import Network.Wai.Handler.Hal
import Test.Tasty
import Test.Tasty.Golden
import Text.Pretty.Simple

test_ConvertProxyRequest :: TestTree
test_ConvertProxyRequest =
  goldenVsString "API Gateway Proxy Request" "test/golden/WaiRequest.txt" $ do
    proxyRequest :: ProxyRequest Void <-
      eitherDecodeFileStrict' "test/data/ProxyRequest.json"
        >>= either fail pure
    waiRequest <- toWaiRequest mempty 443 proxyRequest
    pure . T.encodeUtf8 $ pShowNoColor waiRequest
