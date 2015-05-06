module LocalStorage (getPref, setPref) where

import           Data.Maybe          (fromMaybe)
import           GHCJS.DOM
import           GHCJS.DOM.DOMWindow (domWindowGetLocalStorage)
import           GHCJS.DOM.Storage
import           Safe                (readMay)

getPref :: Read a => String -> a -> IO a
getPref key def =
  do mbWindow <- currentWindow
     case mbWindow of
       Nothing -> return def
       Just win ->
         do Just storage <- domWindowGetLocalStorage win
            fromMaybe def . readMay <$>
              storageGetItem storage key

setPref :: String -> String -> IO ()
setPref key val =
  do mbWindow <- currentWindow
     case mbWindow of
       Nothing -> return ()
       Just win ->
         do Just storage <- domWindowGetLocalStorage win
            storageSetItem storage key val
