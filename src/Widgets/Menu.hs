{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

module Widgets.Menu where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           GHCJS.DOM.HTMLElement
import           GHCJS.Foreign
import           GHCJS.Types
import           LocalStorage            (getPref)
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Class
import           System.FilePath.Posix   (takeBaseName)

import           Example
import           Widgets.Dialog.Location
import           Widgets.Misc            (icon, iconLinkClass)

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(fileSave,"fileSave($1, $2)", JSString -> JSString -> IO ())
JS(dropboxSave,"dropboxSave($1, $2)", JSString -> JSString -> IO ())
JS(enableMenu,"enableMenu($1)", HTMLElement -> IO ())
JS(showModal,"jQuery($1)['modal']('show')",HTMLElement -> IO ())

openMenu :: (MonadWidget t m)
         => El t -> El t -> m (Event t String,Event t String)
openMenu openFileModal locationModal =
  do (menu,dbox) <-
       elAttr' "div" ("class" =: "ui dropdown item") $
       do text "Open"
          icon "dropdown"
          divClass "menu" $
            do divClass "header" (text "Local")
               file <-
                 iconLinkClass "file text" "File" "item"
               performEvent_ $
                 fmap (const . liftIO . void . forkIO $
                       showModal (_el_element openFileModal))
                      file
               divClass "header" (text "Remote")
               loc <-
                 iconLinkClass "world" "Location" "item"
               performEvent_ $
                 fmap (const . liftIO . void . forkIO $
                       showModal (_el_element locationModal))
                      loc
               getDropbox
     liftIO $
       enableMenu (_el_element menu)
     return dbox

makeSaveMenu label source def =
  do (menu,_) <-
       elAttr' "div" ("class" =: "ui dropdown item") $
       do text label
          icon "dropdown"
          divClass "menu" $
            do divClass "header" (text "Local")
               saveToFile <-
                 iconLinkClass "download" "File" "item"
               divClass "header" (text "Remote")
               saveToDropbox <-
                 iconLinkClass "dropbox" "Dropbox" "item"
               content <- holdDyn def source
               performEvent_ $
                 save fileSave (tagDyn content saveToFile)
               performEvent_ $
                 save dropboxSave (tagDyn content saveToDropbox)
     liftIO $
       enableMenu (_el_element menu)

save target =
  fmap (\(format,v) ->
          liftIO . void . forkIO $
          do filename <-
               getPref "Last File" ("untitled.foo")
             let ext =
                   case format of
                     "1preview" -> ".html"
                     otherwise -> "." ++ format
             target (toJSString $ takeBaseName filename ++ ext)
                    (toJSString v))
