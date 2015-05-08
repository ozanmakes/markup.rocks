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

import           Editor
import           Formats
import           Example
import           LocalStorage            (getPref, setPref)
import           Widgets.Menu
import           Widgets.CodeMirror
import           Widgets.Misc            (icon, iconLinkClass, lastDoc, lastExt)
import           Widgets.Setting

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(getTime,"(new Date())['getTime']()", IO Double)
JS(highlightCode,"highlightCode()", IO ())

main :: IO ()
main =
  mainWidget $
  do postGui <- askPostGui
     divClass "ui two column padded grid" $
       do (readerD,t,exts) <-
            divClass "left column" $
            divClass "ui segment" editor
          divClass "right column" $
            divClass "ui segment" $
            do writerD <-
                 divClass "ui top left attached label" $
                 selection def {_selectionConfig_label = "Preview"
                               ,_selectionConfig_initialValue = "1preview"
                               ,_selectionConfig_options = constDyn resultFormats}
               parsed <-
                 $(qDyn [|convertDoc $(unqDyn [|_selection_value readerD|])
                                     $(unqDyn [|_selection_value writerD|])
                                     $(unqDyn [|exts|])
                                     $(unqDyn [|value t|])|])
               result <-
                 forceLossy (updated parsed)
               ext <- liftIO lastExt
               doc <- liftIO lastDoc
               resCM <-
                 divClass "ui top right attached label" $
                 do let output =
                          attachDyn (_selection_value writerD) result
                    makeSaveMenu "Save"
                                 output
                                 (ext,doc)
                    (menu,resCM) <-
                      elAttr' "div"
                              ("class" =: "ui left dropdown compact icon button") $
                      do icon "settings"
                         divClass "menu" $
                           do divClass "header" (text "Result Settings")
                              divClass "item" (setting "CodeMirror Display" True)
                    liftIO $
                      enableMenu (_el_element menu)
                                 (toJSString "nothing")
                    return resCM
               let initial =
                     convertDoc ext "1preview" githubMarkdownExtensions doc
               resultDyn <-
                 holdDyn initial result
               cmEnabled <-
                 liftIO $
                 getPref "CodeMirror Display" True
               cmAttrs <-
                 mapDyn (\w ->
                           case w of
                             "1preview" ->
                               ("style" =: "display: none;" <> "class" =:
                                "outputCM")
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
                               ("style" =: "display: none;" <> "class" =:
                                "output"))
                        (_selection_value writerD)
               elDynAttr "div" htmlAttrs $
                 elDynHtmlAttr' "div"
                                ("class" =: "preview")
                                resultDyn
               performEvent_ $
                 fmap (const . liftIO . void . forkIO $ highlightCode) result
               performEvent_ $
                 fmap (liftIO . void . forkIO . setPref "Last Document" . show)
                      (updated $ value t)

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


stringToWriter :: String -> WriterOptions -> Pandoc -> String
stringToWriter s =
  case s of
    "1preview" -> writeHtmlString
    "ascii" -> writeAsciiDoc
    "html" -> writeHtmlString
    "latex" -> writeLaTeX
    "man" -> writeMan
    "mw" -> writeMediaWiki
    "dw" -> writeDokuWiki
    "dbk" -> writeDocbook
    "odt" -> writeOpenDocument
    "opml" -> writeOPML
    "icml" -> writeICML
    "org" -> writeOrg
    "plain" -> writePlain
    "rst" -> writeRST
    "texinfo" -> writeTexinfo
    "textile" -> writeTextile
    otherwise -> writeMarkdown
