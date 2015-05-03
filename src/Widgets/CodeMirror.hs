{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE TypeFamilies             #-}

module Widgets.CodeMirror where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Ref
import           Data.Default
import           Data.Dependent.Map            (DSum (..))
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLTextAreaElement
import           GHCJS.Foreign
import           GHCJS.Types
import           Reflex
import           Reflex.Dom
import           Reflex.Host.Class

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

newtype CodeMirrorObj =
  CodeMirrorObj {unCodeMirror :: JSRef CodeMirrorObj}

JS(newCodeMirror_, "fromTextArea($1, $2)", HTMLTextAreaElement -> JSFun (JSString -> IO ()) -> IO (JSRef CodeMirrorObj))

JS(toTextArea, "$1['toTextArea']()", CodeMirrorObj -> IO ())
JS(setMode, "$1['setOption']('mode', $2)", CodeMirrorObj -> JSString -> IO ())
JS(setContent, "$1['setValue']($2)", CodeMirrorObj -> JSString -> IO ())

newCodeMirrorObj :: HTMLTextAreaElement -> (String -> IO ()) -> IO CodeMirrorObj
newCodeMirrorObj textarea onChange =
  do onChangeFun <-
       syncCallback1 AlwaysRetain
                     True
                     (onChange . fromJSString)
     liftM CodeMirrorObj $
       newCodeMirror_ textarea onChangeFun

data CodeMirrorConfig t =
  CodeMirrorConfig {_codeMirrorConfig_initialValue :: String
                   ,_codeMirrorConfig_enableCodeMirror :: Event t Bool
                   ,_codeMirrorConfig_changeLang :: Event t String
                   ,_codeMirrorConfig_setValue :: Event t String
                   ,_codeMirrorConfig_attributes :: Dynamic t (Map String String)}

instance Reflex t => Default (CodeMirrorConfig t) where
  def =
    CodeMirrorConfig {_codeMirrorConfig_initialValue = ""
                     ,_codeMirrorConfig_enableCodeMirror = never
                     ,_codeMirrorConfig_changeLang = never
                     ,_codeMirrorConfig_setValue = never
                     ,_codeMirrorConfig_attributes = constDyn mempty}

data CodeMirror t =
  CodeMirror {_codeMirror_value :: Dynamic t String}

instance HasValue (CodeMirror t) where
  type Value (CodeMirror t) = Dynamic t String
  value = _codeMirror_value

codeMirror :: MonadWidget t m
           => CodeMirrorConfig t -> m (CodeMirror t)
codeMirror (CodeMirrorConfig initial eCM eLang eSet attrs) =
  do e <-
       liftM castToHTMLTextAreaElement $
       buildEmptyElement "textarea" =<<
       mapDyn (Map.insert "class" "input") attrs
     postGui <- askPostGui
     runWithActions <- askRunWithActions
     (eRecv,eRecvTriggerRef) <- newEventWithTriggerRef
     let onChange m =
           maybe (return ())
                 (\t ->
                    postGui $
                    runWithActions [t :=> m]) =<<
           readRef eRecvTriggerRef
     mvar <-
       liftIO $
       do htmlTextAreaElementSetValue e initial
          newCodeMirrorObj e onChange >>=
            newMVar
     performEvent_ $
       fmap (\v ->
               liftIO $
               do maybeCodeMirror <- tryTakeMVar mvar
                  case maybeCodeMirror of
                    Just cm -> toTextArea cm
                    Nothing ->
                      do cm <-
                           newCodeMirrorObj e onChange
                         putMVar mvar cm)
            eCM
     performEvent_ $
       fmap (\v ->
               liftIO $
               do maybeCodeMirror <- tryTakeMVar mvar
                  case maybeCodeMirror of
                    Just cm ->
                      do setMode cm . toJSString . langToMode $ v
                         putMVar mvar cm
                    Nothing -> return ())
            eLang
     performEvent_ $
       fmap (\v ->
               liftIO $
               do maybeCodeMirror <- tryTakeMVar mvar
                  case maybeCodeMirror of
                    Just cm ->
                      do setContent cm . toJSString $ v
                         putMVar mvar cm
                    Nothing ->
                      htmlTextAreaElementSetValue e v)
            eSet
     ev <-
       wrapDomEvent e elementOninput $
       liftIO $ htmlTextAreaElementGetValue e
     v <-
       holdDyn initial (leftmost [eSet,eRecv,ev])
     return $
       CodeMirror v

langToMode :: String -> String
langToMode lang =
  case lang of
    "md" -> "gfm"
    "html" -> "htmlmixed"
    "textile" -> "textile"
    "rst" -> "rst"
    "man" -> "troff"
    _ -> "null"
