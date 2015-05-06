{-# LANGUAGE CPP         #-}
{-# LANGUAGE RecursiveDo #-}

module Widgets.Dialog.Location where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Ref
import           Data.Dependent.Map     (DSum (..))
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Monoid            ((<>))
import           Data.Text              (unpack)
import           GHCJS.Foreign
import           GHCJS.Types
import           Network.URI            (isAbsoluteURI)
import           Reflex
import           Reflex.Dom
import           Reflex.Host.Class
import           Safe                   (tailSafe)
import           System.FilePath.Posix  (takeExtension, takeFileName)

import           LocalStorage           (setPref)
import           Widgets.Misc           (iconLinkClass)

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(dropboxOpen,"dropboxOpen($1)", JSFun (JSString -> IO ()) -> IO ())
JS(dropboxSave,"dropboxSave($1, $2)", JSString -> JSString -> IO ())
JS(hideModal,"jQuery('.modal.active')['modal']('hide')",IO ())

locationDialog :: MonadWidget t m => m (El t, (Event t String, Event t String))
locationDialog =
  elAttr' "div" ("class" =: "ui small modal") $
  do divClass "header" (text "Open Location")
     rec (form,urlBox) <-
           elAttr' "div" ("class" =: "ui form") $
           do divClass "field" $
                do textInput $
                     TextInputConfig "url" "" (fmap (const "") result) (constDyn ("placeholder" =: "http://"))
         (events, result) <- divClass "actions" $
          do divClass "ui negative button" (text "No")
             openButton <-
               iconLinkClass "checkmark" "Open" "ui right labeled icon button"
             result <-
               getURL $
               ffilter isAbsoluteURI $
               tag (current (value urlBox)) $
               leftmost [openButton,textInputGetEnter urlBox]
             performEvent_ $ fmap (const . liftIO $ hideModal) result

             return (ffilter (isJust . snd) $ result, result)
     return (fmap (fromMaybe "" . snd) events, fmap fst events)

getDropbox :: (MonadWidget t m) => m (Event t String, Event t String)
getDropbox =
  do link <-
       iconLinkClass "dropbox" "Dropbox" "item"
     postGui <- askPostGui
     runWithActions <- askRunWithActions
     (eRecv,eRecvTriggerRef) <- newEventWithTriggerRef
     callback <-
       liftIO $
       syncCallback1
         AlwaysRetain
         True
         (\jsUrl ->
            do let val = fromJSString jsUrl
               maybe (return ())
                     (\t ->
                        postGui $
                        runWithActions
                          [t :=>
                           Just val]) =<<
                 readRef eRecvTriggerRef)
     result <-
       getURL $
       fmapMaybe id eRecv
     performEvent_ $
       fmap (const . liftIO $ dropboxOpen callback) link
     let events =
           ffilter (isJust . snd) $
           result
     return (fmap (fromMaybe "" .
                   snd)
                  events
            ,fmap fst events)

getURL :: (MonadWidget t m) => Event t String -> m (Event t (String, Maybe String))
getURL url =
  do r <-
       performRequestAsync $
       fmap (\x -> XhrRequest "GET" x def) url
     let resp = fmap decodeXhrResponse r
     ext <-
       holdDyn "md" $
       fmap (tailSafe . takeExtension) url
     performEvent_ $
       fmap (liftIO .
             setPref "Last File" .
             show .
             takeFileName)
            url
     return $
       attachDyn ext resp
  where decodeXhrResponse = processXhrResponse . fmap unpack . _xhrResponse_body


processXhrResponse :: Maybe String -> Maybe String
processXhrResponse (Just "") = Nothing
processXhrResponse (Just ('C':'a':'n':'n':'o':'t':' ':'G':'E':'T':_)) = Nothing
processXhrResponse (Just r) = Just r
processXhrResponse Nothing = Nothing

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (x,y) = (x,f y)
