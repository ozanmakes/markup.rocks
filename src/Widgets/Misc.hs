module Widgets.Misc where

import           Control.Applicative   ((<$>))
import           Reflex.Dom
import           Safe                  (tailSafe)
import           System.FilePath.Posix (takeExtension)

import           Example               (markdownExample)
import           LocalStorage          (getPref)

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

lastExt :: IO String
lastExt = (tailSafe . takeExtension) <$> getPref "Last File" "untitled.md"

lastDoc :: IO String
lastDoc = getPref "Last Document" markdownExample
