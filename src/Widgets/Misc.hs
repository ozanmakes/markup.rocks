module Widgets.Misc where

import           Reflex.Dom

iconLinkClass :: MonadWidget t m
              => String -> String -> String -> m (Event t ())
iconLinkClass i s c =
  do (l,_) <-
       elAttr' "a" ("class" =: c) $
       do icon i
          text s
     return $ _el_clicked l

icon :: MonadWidget t m => String -> m ()
icon i = elClass "i" (i ++ " icon") (return ())
