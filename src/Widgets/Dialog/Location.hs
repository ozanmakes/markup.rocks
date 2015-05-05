{-# LANGUAGE CPP #-}

module Widgets.Dialog.Location where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Ref
import           Data.Dependent.Map     (DSum (..))
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Monoid            ((<>))
import           Data.Text              (unpack)
import           GHCJS.Foreign
import           GHCJS.Types
import           Reflex
import           Reflex.Dom
import           Reflex.Host.Class
import           Safe                   (tailSafe)
import           System.FilePath.Posix  (takeExtension)

import           Widgets.Misc           (iconLinkClass)

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(dropboxFile,"dropboxFile($1)", JSFun (JSString -> IO ()) -> IO ())


locationDialog :: MonadWidget t m => m (El t, (Event t String, Event t String))
locationDialog =
  elAttr' "div" ("class" =: "ui small modal") $
  do divClass "header" (text "Open Location")
     (form,urlBox) <-
       elAttr' "form" ("class" =: "ui form") $
       do divClass "field" $
            do textInput def {_textInputConfig_inputType = "url"
                             ,_textInputConfig_attributes =
                                constDyn (("placeholder" =: "http://") <>
                                          ("required" =: "required") <>
                                          ("pattern" =: "https?://.+"))}
     divClass "actions" $
       do divClass "ui negative button" (text "No")
          openButton <-
            iconLinkClass "checkmark" "Open" "ui right labeled icon button"
          result <-
            getURL $
            tag (current (value urlBox)) openButton
          let events =
                ffilter (isJust . snd) $
                result
          return (fmap (fromMaybe "" .
                        snd)
                       events
                 ,fmap fst events)

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
       fmap (const . liftIO $ dropboxFile callback) link
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
     ext <- holdDyn "md" $ fmap (tailSafe . takeExtension) url
     return $ attachDyn ext resp
  where decodeXhrResponse = processXhrResponse . fmap unpack . _xhrResponse_body


processXhrResponse :: Maybe String -> Maybe String
processXhrResponse (Just "") = Nothing
processXhrResponse (Just ('C':'a':'n':'n':'o':'t':' ':'G':'E':'T':_)) = Nothing
processXhrResponse (Just r) = Just r
processXhrResponse Nothing = Nothing

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (x,y) = (x,f y)
