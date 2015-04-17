module Handler.Home where

import Text.Blaze.Html  ( Html )
import Yesod.Core       ( defaultLayout )

import Import


getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "homepage")
