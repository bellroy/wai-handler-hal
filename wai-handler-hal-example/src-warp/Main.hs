module Main where

import qualified Example
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.runEnv 8080 Example.app
