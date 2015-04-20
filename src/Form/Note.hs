module Form.Note where

import Prelude.Extended
import Text.Blaze.Html  ( Html )
import Yesod.Form       ( FormResult, MForm, Textarea (..)
                        , areq, renderDivs, textareaField
                        )

import Foundation       ( Handler, Widget )


noteContentForm ::
    Maybe Text ->
    Html -> MForm Handler (FormResult Textarea, Widget)
noteContentForm mContentOld = renderDivs $
    areq textareaField  "" (Textarea <$> mContentOld)
