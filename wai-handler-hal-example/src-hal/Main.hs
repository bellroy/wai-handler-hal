module Main where

import AWS.Lambda.Context (runReaderTLambdaContext)
import AWS.Lambda.Runtime (mRuntimeWithContext)
import qualified Data.Vault.Lazy as Vault
import qualified Example
import qualified Network.Wai.Handler.Hal as WaiHandler

main :: IO ()
main =
  runReaderTLambdaContext . mRuntimeWithContext $
    WaiHandler.run Vault.empty 443 Example.app
