module Form where

import Import


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
