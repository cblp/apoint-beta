module Widget where

import Form

import Import


makePostButton :: Route App -> Text -> Handler Widget
makePostButton route label = do
    (formWidget, enctype) <- generateFormPost emptyForm
    return [whamlet|
        <form method=post action=@{route} enctype=#{enctype}>
            ^{formWidget}
            <button .btn type=submit>
                #{label}
        |]
