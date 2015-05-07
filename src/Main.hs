{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE RecursiveDo              #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool               (bool)
import           Data.Default
import           Data.Either
import           Data.List
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           GHCJS.DOM.HTMLElement
import           GHCJS.Foreign
import           GHCJS.Types
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Class
import           Reflex.Dynamic.TH
import           Text.Pandoc

import           Formats
import           LocalStorage            (getPref)
import           Widgets.CodeMirror
import           Widgets.Dialog.Location
import           Widgets.Dialog.OpenFile
import           Widgets.Misc            (icon, iconLinkClass)
import           Widgets.Setting

data Component = Reader | Writer

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(getTime,"(new Date())['getTime']()", IO Double)
JS(enableMenu,"enableMenu($1)", HTMLElement -> IO ())
JS(highlightCode,"highlightCode()", IO ())
JS(showModal,"jQuery($1)['modal']('show')",HTMLElement -> IO ())
JS(fileSave,"fileSave($1, $2)", JSString -> JSString -> IO ())
JS(dropboxSave,"dropboxSave($1, $2)", JSString -> JSString -> IO ())

main :: IO ()
main =
  mainWidget $
  do postGui <- askPostGui
     (locationModal,(locationContents,locationExt)) <- locationDialog
     (openFileModal,(fileContents, fileExt)) <- openFileDialog
     rec ((openMenu,saveMenu),(dropboxContents,dropboxExt)) <-
           divClass "ui fixed inverted menu" $
           do divClass "header brand item" (text "markup.rocks")
              (openMenu,dbox) <-
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
              (saveMenu,_) <-
                elAttr' "div" ("class" =: "ui dropdown item") $
                do text "Save"
                   icon "dropdown"
                   divClass "menu" $
                     do divClass "header" (text "Local")
                        saveToFile <-
                          iconLinkClass "download" "File" "item"
                        divClass "header" (text "Remote")
                        saveToDropbox <-
                          iconLinkClass "dropbox" "Dropbox" "item"
                        content <- holdDyn ("md",markdownExample) output
                        performEvent_ $
                          fmap (\(ext,v) ->
                                  liftIO . void . forkIO $
                                  do filename <- getPref "Last File" ("untitled." ++ ext)
                                     dropboxSave (toJSString filename)
                                                 (toJSString v))
                               (tagDyn content saveToDropbox)
                        performEvent_ $
                          fmap (\(ext,v) ->
                                  liftIO . void . forkIO $
                                  do filename <- getPref "Last File" ("untitled." ++ ext)
                                     fileSave (toJSString filename)
                                              (toJSString v))
                               (tagDyn content saveToFile)
              return ((openMenu,saveMenu),dbox)
         liftIO $
           do enableMenu (_el_element openMenu)
              enableMenu (_el_element saveMenu)
         output <-
           divClass "ui two column padded grid" $
           do (dropzone,(readerD,t,exts)) <-
                divClass "left column" $
                elAttr' "div" ("class" =: "ui piled segment") $
                editor (leftmost [locationContents,dropboxContents, fileContents])
                       (leftmost [locationExt,dropboxExt, fileExt])
              divClass "right column" $
                divClass "ui piled segment" $
                do writerD <-
                     divClass "ui top left attached label" $
                     selection def {_selectionConfig_label = "Preview"
                                   ,_selectionConfig_initialValue = "1preview"
                                   ,_selectionConfig_options = constDyn resultFormats}
                   resCM <- divClass "ui top right attached label" $
                     divClass "ui icon simple left dropdown compact button" $
                     do icon "settings"
                        divClass "menu" $
                          do divClass "header" (text "Result Settings")
                             divClass "item" (setting "CodeMirror Display" True)
                   parsed <-
                     $(qDyn [|convertDoc $(unqDyn [|_selection_value readerD|])
                                         $(unqDyn [|_selection_value writerD|])
                                         $(unqDyn [|exts|])
                                         $(unqDyn [|value t|])|])
                   result <-
                     forceLossy (updated parsed)
                   let initial =
                         convertDoc "md" "1preview" githubMarkdownExtensions markdownExample
                   resultDyn <-
                     holdDyn initial result

                   cmEnabled <- liftIO $ getPref "CodeMirror Display" True
                   cmAttrs <-
                     mapDyn (\w ->
                               case w of
                                 "1preview" ->
                                   ("style" =: "display: none;" <> "class" =: "outputCM")
                                 otherwise ->
                                   ("class" =: "outputCM"))
                            (_selection_value writerD)
                   elDynAttr "div" cmAttrs $
                     codeMirror
                       def {_codeMirrorConfig_initialValue = initial
                           ,_codeMirrorConfig_enabled = cmEnabled
                           ,_codeMirrorConfig_enableCodeMirror =
                              updated (_setting_value resCM)
                           ,_codeMirrorConfig_changeLang =
                              updated (_selection_value writerD)
                           ,_codeMirrorConfig_setValue = result}
                   htmlAttrs <-
                     mapDyn (\w ->
                               case w of
                                 "1preview" ->
                                   ("class" =: "output")
                                 otherwise ->
                                   ("style" =: "display: none;" <> "class" =: "output"))
                            (_selection_value writerD)
                   elDynAttr "div" htmlAttrs $
                     elDynHtml' "div" resultDyn
                   performEvent_ $
                     fmap (const . liftIO . void . forkIO $ highlightCode) result
                   return $
                     attachDyn (_selection_value writerD) result
     return ()
forceLossy :: (MonadWidget t m,NFData a)
           => Event t a -> m (Event t a)
forceLossy e =
  do mvar <- liftIO newEmptyMVar
     diffsMVar <- liftIO (newMVar [])
     performEventAsync (fmap (callAtNextInterval mvar diffsMVar) e)
  where callAtNextInterval mvar diffsMVar e cb =
          void . liftIO $
          do maybeThreadId <- tryTakeMVar mvar
             case maybeThreadId of
               Just threadId -> killThread threadId
               Nothing -> return ()
             threadId <-
               forkIO $
               do start <- getTime
                  diffs <- readMVar diffsMVar
                  let avg =
                        round $ sum diffs / genericLength diffs :: Int
                  when (avg > 500)
                       (threadDelay (min (avg * 1000) 1000000))
                  cb $!! e
                  end <- getTime
                  let diff = end - start
                  let appendTime =
                        return .
                        (subtract start end :) .
                        take 2 .
                        reverse
                  modifyMVar_ diffsMVar appendTime
             putMVar mvar threadId

editor :: (MonadWidget t m)
       => Event t String
       -> Event t String
       -> m (Selection t,CodeMirror t,Dynamic t (Set Extension))
editor eSet readerSet =
  do d <-
       divClass "ui top left attached label" $
       selection $
       SelectionConfig "md"
                       "Markdown"
                       (constDyn sourceFormats)
                       readerSet
     (advancedEditor,exts) <-
       divClass "ui top right attached label" $
       divClass "ui icon simple left dropdown compact button" $
       do icon "settings"
          divClass "menu" $
            do divClass "header" (text "Source Settings")
               advancedEditor <-
                 divClass "item" $
                 setting "CodeMirror Editor" True
               exts <- extensions Reader "md"
               return (advancedEditor,exts)
     cmEnabled <- liftIO $ getPref "CodeMirror Editor" True
     t <-
       codeMirror
         def {_codeMirrorConfig_initialValue = markdownExample
             ,_codeMirrorConfig_enabled = cmEnabled
             ,_codeMirrorConfig_enableCodeMirror =
                updated (_setting_value advancedEditor)
             ,_codeMirrorConfig_changeLang =
                updated (_selection_value d)
             ,_codeMirrorConfig_setValue = eSet}
     return (d,t,exts)

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

convertDoc :: String -> String -> Set Extension -> String -> String
convertDoc readerStr writerStr extensions t =
  either (const "") writer parsed
  where parsed = reader t
        writer = stringToWriter writerStr def
        reader =
          stringToReader
            readerStr
            def {readerApplyMacros = False
                ,readerExtensions = extensions}

stringToExtensions :: Component
                   -> String
                   -> [(String,Set Extension -> Set Extension)]
stringToExtensions Reader "md" =
  [("Hard Line Breaks",Set.insert Ext_hard_line_breaks)
  ,("GitHub Flavored",Set.union githubMarkdownExtensions)]
stringToExtensions _ _ = []

stringToWriter :: String -> WriterOptions -> Pandoc -> String
stringToWriter s =
  case s of
    "1preview" -> writeHtmlString
    "ascii" -> writeAsciiDoc
    "html" -> writeHtmlString
    "latex" -> writeLaTeX
    "man" -> writeMan
    "org" -> writeOrg
    "plain" -> writePlain
    "rst" -> writeRST
    "texinfo" -> writeTexinfo
    "textile" -> writeTextile
    otherwise -> writeMarkdown

markdownExample =
  unlines ["Heading"
          ,"======="
          ,""
          ,"Sub-heading"
          ,"-----------"
          ,""
          ,"### Another deeper heading"
          ,""
          ,"Paragraphs are separated"
          ,"by a blank line."
          ,""
          ,"Leave 2 spaces at the end of a line to do a  "
          ,"line break"
          ,""
          ,"Text attributes *italic*, **bold**,"
          ,"`monospace`, ~~strikethrough~~ ."
          ,""
          ,"A [link](http://example.com)."
          ,""
          ,"Shopping list:"
          ,""
          ,"  * apples"
          ,"  * oranges"
          ,"  * pears"
          ,""
          ,"Numbered list:"
          ,""
          ,"  1. apples"
          ,"  2. oranges"
          ,"  3. pears"
          ,""
          ,"The rain---not the reign---in"
          ,"Spain."]

defaultExtensions :: Set Extension
defaultExtensions =
  Set.difference pandocExtensions
                 (Set.fromList [Ext_raw_tex,Ext_latex_macros])
