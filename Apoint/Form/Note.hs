module Form.Note where

import Data.Functor ( (<$>) )
import Data.Text    ( Text )
import Prelude

import Import


noteContentForm ::
    Maybe Text ->
    Html -> MForm Handler (FormResult Textarea, Widget)
noteContentForm mContentOld = renderDivs $
    areq textareaField  "" (Textarea <$> mContentOld)
