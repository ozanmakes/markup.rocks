{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE TemplateHaskell          #-}

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
import           Text.Pandoc.Error       (PandocError)

import           Widgets.CodeMirror
import           Widgets.Dialog.Location
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

main :: IO ()
main =
  mainWidget $
  do postGui <- askPostGui
     (locationModal,locationContents) <- locationDialog
     (menu,_) <-
       divClass "ui fixed inverted menu" $
       do divClass "header brand item" (text "markup.rocks")
          elAttr' "div" ("class" =: "ui dropdown item") $
            do text "Open"
               icon "dropdown"
               divClass "menu" $
                 do divClass "header" (text "Local")
                    iconLinkClass "file text" "File" "item"
                    divClass "header" (text "Remote")
                    loc <-
                      iconLinkClass "world" "Location" "item"
                    performEvent_ $
                      fmap (const . liftIO . void . forkIO $
                            showModal (_el_element locationModal))
                           loc
     liftIO $
       enableMenu (_el_element menu)
     divClass "ui two column padded grid" $
       do (readerD,t,exts) <-
            divClass "left column" (editor locationContents)
          divClass "right column" $
            divClass "ui piled segment" $
            do writerD <-
                 divClass "ui top left attached label" $
                 selection "Preview" "1preview" (constDyn resultFormats)
               divClass "ui top right attached label" $
                 divClass "ui icon simple left dropdown compact button" $
                 do icon "settings"
                    divClass "menu" $
                      divClass "header" (text "Result Settings")
               parsed <-
                 $(qDyn [|convertDoc $(unqDyn [|_selection_value readerD|])
                                     $(unqDyn [|_selection_value writerD|])
                                     $(unqDyn [|exts|])
                                     $(unqDyn [|value t|])|])
               result <-
                 forceLossy (updated parsed)
               let initial =
                     convertDoc "md" "1preview" githubMarkdownExtensions markdownExample
               resultDyn <- holdDyn initial result
               elDynHtmlAttr' "div"
                              ("class" =: "output")
                              resultDyn
               performEvent_ $
                 fmap (const . liftIO . void . forkIO $ highlightCode) result
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
       -> m (Selection t,CodeMirror t,Dynamic t (Set Extension))
editor eSet =
  divClass "ui piled segment" $
  do d <-
       divClass "ui top left attached label" $
       selection "Markdown" "md" (constDyn sourceFormats)
     (advancedEditor,exts) <-
       divClass "ui top right attached label" $
       divClass "ui icon simple left dropdown compact button" $
       do icon "settings"
          divClass "menu" $
            do divClass "header" (text "Source Settings")
               advancedEditor <-
                 divClass "item" $
                 setting "CodeMirror"
               exts <- extensions Reader "md"
               return (advancedEditor,exts)
     t <-
       codeMirror
         def {_codeMirrorConfig_initialValue = markdownExample
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
                         setting label
                       mapDyn (bool id modifier)
                              (_setting_value s))
                 (stringToExtensions component "md")
          mconcatDyn exts
     $(qDyn [|$(unqDyn [|exts|]) defaultExtensions|])

convertDoc :: String -> String -> Set Extension -> String -> String
convertDoc readerStr writerStr extensions t =
  either (const "") writer parsed
  where parsed = reader t
        writer =
          wrapResult writerStr .
          stringToWriter writerStr def
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
  ,("GitHub Flavored",Set.union githubMarkdownExtensions)
  ,("MultiMarkdown"
   ,Set.delete Ext_raw_tex .
    Set.union multimarkdownExtensions)]
stringToExtensions _ _ = []

sourceFormats :: Map String String
sourceFormats =
  Map.fromList
    [("docbook","DocBook")
    ,("html","HTML")
    ,("latex","LaTeX")
    ,("md","Markdown")
    ,("mw","MediaWiki")
    ,("opml","OPML")
    ,("org","Org")
    ,("rst","reStructuredText")
    ,("t2t","txt2tags")
    ,("textile","Textile")
    ,("twiki","TWiki")]

stringToReader :: String -> ReaderOptions -> String -> Either PandocError Pandoc
stringToReader s =
  case s of
    "docbook" -> readDocBook
    "html" -> readHtml
    "latex" -> readLaTeX
    "mw" -> readMediaWiki
    "opml" -> readOPML
    "org" -> readOrg
    "rst" -> readRST
    "t2t" -> readTxt2TagsNoMacros
    "textile" -> readTextile
    "twiki" -> readTWiki
    otherwise -> readMarkdown

resultFormats :: Map String String
resultFormats =
  Map.fromList
    [("1preview","Preview")
    ,("ascii","AsciiDoc")
    ,("html","HTML")
    ,("latex","LaTeX")
    ,("man","Man")
    ,("md","Markdown")
    ,("org","Org")
    ,("plain","Plaintext")
    ,("rst","reStructuredText")
    ,("texinfo","Texinfo")
    ,("textile","Textile")]

wrapResult :: String -> String -> String
wrapResult "1preview" s = "<div class=\"preview\">" ++ s ++ "</div>"
wrapResult _ s = "<textarea>" ++ s ++ "</textarea>"

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
