module Form where

import Control.Applicative    ( pure )
import Prelude
import Text.Blaze.Html        ( Html )
import Text.Shakespeare.I18N  ( RenderMessage )
import Yesod.Core             ( HandlerSite, MonadHandler, invalidArgs )
import Yesod.Form             ( FormMessage
                              , FormResult  ( FormFailure
                                            , FormMissing
                                            , FormSuccess
                                            )
                              , MForm
                              , renderDivs, runFormPost
                              )

import Foundation             ( Handler, Widget )


emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()


runFormPostChecked :: forall (m :: * -> *) b xml.
                            (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) =>
                            (Html
                             -> MForm m (FormResult b, xml))
                            -> m b
runFormPostChecked form = do
    ((formResult, _), _) <- runFormPost form
    case formResult of
        FormSuccess a       -> return a
        FormFailure errors  -> invalidArgs errors
        FormMissing         -> invalidArgs ["FormMissing"]
