module Main where

import AWS.Lambda.Runtime (mRuntime)
import qualified Example
import qualified Network.Wai.Handler.Hal as WaiHandler

main :: IO ()
main = mRuntime $ WaiHandler.run Example.app
