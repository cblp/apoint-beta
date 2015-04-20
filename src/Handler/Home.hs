module Handler.Home where

import Text.Blaze.Html  ( Html )
import Yesod.Core       ( defaultLayout )

import Foundation       ( Handler )
import Settings         ( widgetFile )


getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "homepage")
