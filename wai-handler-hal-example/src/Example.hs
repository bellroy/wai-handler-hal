{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Example where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Servant.API
import Servant.Server (Application, Server, serve)

type Api = Greet :<|> Hoot

type Greet =
  "greet"
    :> QueryParam' '[Required, Strict] "person" Text
    :> Get '[JSON] Message

type Hoot = "hoot" :> Get '[JSON] Message

greet :: Server Greet
greet person = pure . Message $ "Hello, " <> person <> "!"

hoot :: Server Hoot
hoot = pure $ Message "Hoot!"

newtype Message = Message Text

instance ToJSON Message where
  toJSON (Message t) = object ["message" .= t]

app :: Application
app = serve (Proxy @Api) $ greet :<|> hoot
