{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import AWS.Lambda.Context (runReaderTLambdaContext)
import AWS.Lambda.Runtime (mRuntimeWithContext)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Handler.Hal as WaiHandler
import Servant.API
import Servant.Server (Application, Server, serve)

type Api = Greet :<|> Hoot

type Greet =
  "greet"
    :> QueryParam' '[Required, Strict] "person" Text
    :> Get '[JSON] Message

greet :: Server Greet
greet person = pure . Message $ "Hello, " <> person <> "!"

type Hoot = "hoot" :> Get '[JSON] Message

hoot :: Server Hoot
hoot = pure $ Message "Hoot!"

newtype Message = Message Text

instance ToJSON Message where
  toJSON (Message t) = object ["message" .= t]

app :: Application
app = serve (Proxy @Api) $ greet :<|> hoot

main :: IO ()
main =
  runReaderTLambdaContext . mRuntimeWithContext $
    WaiHandler.run Vault.empty 443 app
