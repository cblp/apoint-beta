module Form.Note where

import Import


noteContentForm ::
    Maybe Text ->
    Html -> MForm Handler (FormResult Textarea, Widget)
noteContentForm mContentOld = renderDivs $
    areq textareaField  "" (Textarea <$> mContentOld)