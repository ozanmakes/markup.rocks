{-# LANGUAGE CPP         #-}

module Widgets.Dialog.OpenFile where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Ref
import           Data.Dependent.Map     (DSum (..))
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Monoid            ((<>))
import           GHCJS.DOM.HTMLInputElement
import           GHCJS.Foreign
import           GHCJS.Types
import           Reflex
import           Reflex.Dom
import           Reflex.Host.Class
import           Safe                   (tailSafe)
import           System.FilePath.Posix  (takeExtension)

import           LocalStorage           (setPref)
import           Widgets.Misc           (iconLinkClass)

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(fileOpen,
   "fileOpen($1, $2)",
   HTMLInputElement -> JSFun (JSString -> JSString -> IO ()) -> IO ())
JS(hideModal,"jQuery('.modal.active')['modal']('hide')",IO ())

openFileDialog :: MonadWidget t m
               => m (El t,(Event t String,Event t String))
openFileDialog =
  elAttr' "div" ("class" =: "ui basic modal") $
  do divClass "header" (text "Open File")
     divClass "ui form" $
       do divClass "field" $
            do fileBox <-
                 textInput def {_textInputConfig_inputType = "file"
                               ,_textInputConfig_attributes =
                                  constDyn ("enctype" =: "multipart/form-data" <>
                                            "style" =: "font-size: 34px;")}
               postGui <- askPostGui
               runWithActions <- askRunWithActions
               (eRecv,eRecvTriggerRef) <- newEventWithTriggerRef
               callback <-
                 liftIO $
                 syncCallback2
                   AlwaysRetain
                   True
                   (\name contents ->
                      maybe (return ())
                            (\t ->
                               postGui $
                               runWithActions
                                 [t :=>
                                  Just (fromJSString name,fromJSString contents)]) =<<
                      readRef eRecvTriggerRef)
               liftIO $
                 fileOpen (_textInput_element fileBox) callback
               let ev = fmapMaybe id eRecv
               performEvent_ $
                 fmap (\(filename,_) ->
                         liftIO $
                         do setPref "Last File" (show filename)
                            hideModal)
                      ev
               return (fmap snd ev,fmap (tailSafe . takeExtension . fst) ev)
