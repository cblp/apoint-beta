module Widget where

import Prelude

import Form ( emptyForm )
import Import


makePostButton :: Route App -> Html -> Handler Widget
makePostButton route label = do
    (formWidget, enctype) <- generateFormPost emptyForm
    formId <- newIdent
    return [whamlet|
        <button .btn onClick="$('##{formId}').submit()">
            <form method=post action=@{route} enctype=#{enctype} ##{formId}
                    style="display: none;">
                ^{formWidget}
            #{label}
        |]
