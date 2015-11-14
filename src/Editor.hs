{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE GADTs                    #-}

module Editor where

import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Class
import           Control.Monad.IO.Class
import           Data.Set                (Set)
import           Text.Pandoc
import           Reflex.Dynamic.TH
import           Data.Bool               (bool)
import           Data.Map                (findWithDefault)
import qualified Data.Set                as Set
import           GHCJS.Foreign

import           LocalStorage            (getPref)
import           Widgets.CodeMirror
import           Widgets.Dialog.Location
import           Widgets.Dialog.OpenFile
import           Example
import           Widgets.Setting
import           Formats
import           Widgets.Menu
import           Widgets.Misc            (icon, lastDoc, lastExt)

data Component = Reader | Writer

editor :: (MonadWidget t m)
       => m (Selection t,CodeMirror t,Dynamic t (Set Extension))
editor =
  do (openFileModal,(fileContents,fileExt)) <- openFileDialog
     (locationModal,(locationContents,locationExt)) <- locationDialog
     rec ext <- liftIO lastExt
         doc <- liftIO lastDoc
         d <-
           divClass "ui top left attached label" $
           selection $
           SelectionConfig ext
                           (findWithDefault "Markdown" ext resultFormats)
                           (constDyn sourceFormats)
                           (leftmost [locationExt,dropboxExt, fileExt])
         ((advancedEditor,insertSp,exts), (dropboxContents,dropboxExt)) <-
           divClass "ui top right attached label" $
           do dbox <- openMenu openFileModal locationModal
              let input = attachDyn (_selection_value d) (updated $ value t)
              makeSaveMenu "Save" input (ext,doc)
              s <- settings
              return (s, dbox)
         cmEnabled <-
           liftIO $
           getPref "CodeMirror Editor" True
         t <-
           codeMirror
             def {_codeMirrorConfig_initialValue = doc
                 ,_codeMirrorConfig_enabled = cmEnabled
                 ,_codeMirrorConfig_enableCodeMirror =
                    updated advancedEditor
                 ,_codeMirrorConfig_insertSpaces =
                    updated insertSp
                 ,_codeMirrorConfig_changeLang =
                    updated (value d)
                 ,_codeMirrorConfig_setValue =
                    leftmost [locationContents,dropboxContents, fileContents]}
     return (d,t,exts)

settings :: (MonadWidget t m)
         => m (Dynamic t Bool, Dynamic t Bool, Dynamic t (Set Extension))
settings =
  do (menu,children) <-
       elAttr' "div" ("class" =: "ui left dropdown compact icon button") $
       do icon "settings"
          divClass "menu" $
            do divClass "header" (text "Source Settings")
               (advancedEditor,insertSp) <-
                 divClass "item" $
                 do advancedEditor <-
                      setting "CodeMirror Editor" True
                    insertSp <-
                      divClass "left menu" $
                      do divClass "item" $
                           setting "Use spaces for indentation" False
                    return ((value advancedEditor),(value insertSp))
               divClass "header" (text "Markdown")
               exts <- extensions Reader "md"
               return (advancedEditor,insertSp,exts)
     liftIO $
       enableMenu (_el_element menu)
                  (toJSString "nothing")
     return children

extensions :: (MonadWidget t m)
           => Component -> String -> m (Dynamic t (Set Extension))
extensions component lang =
  do exts <-
       do exts <-
            mapM (\(label,modifier) ->
                    do s <-
                         divClass "item" $
                         setting label False
                       mapDyn (bool id modifier)
                              (_setting_value s))
                 (stringToExtensions component "md")
          mconcatDyn exts
     $(qDyn [|$(unqDyn [|exts|]) defaultExtensions|])

stringToExtensions :: Component
                   -> String
                   -> [(String,Set Extension -> Set Extension)]
stringToExtensions Reader "md" =
  [("Hard Line Breaks",Set.insert Ext_hard_line_breaks)
  ,("GitHub Flavored"
   ,Set.delete Ext_hard_line_breaks .
    Set.delete Ext_emoji .
    Set.union githubMarkdownExtensions)]
stringToExtensions _ _ = []

defaultExtensions :: Set Extension
defaultExtensions =
  Set.difference pandocExtensions
                 (Set.fromList [Ext_raw_tex,Ext_latex_macros])
