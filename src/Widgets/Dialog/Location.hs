module Widgets.Dialog.Location where

import           Data.Monoid  ((<>))
import           Data.Text    (unpack)
import           Reflex
import           Reflex.Dom

import           Widgets.Misc (iconLinkClass)

locationDialog :: MonadWidget t m => m (El t, Event t String)
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
          return $
            fmapMaybe id result

getURL :: (MonadWidget t m) => Event t String -> m (Event t (Maybe String))
getURL url =
  do r <-
       performRequestAsync $
       fmap (\x -> XhrRequest "GET" x def) url
     return $
       fmap decodeXhrResponse r
  where decodeXhrResponse = processXhrResponse . fmap unpack . _xhrResponse_body


processXhrResponse :: Maybe String -> Maybe String
processXhrResponse (Just "") = Nothing
processXhrResponse (Just ('C':'a':'n':'n':'o':'t':' ':'G':'E':'T':_)) = Nothing
processXhrResponse (Just r) = Just r
processXhrResponse Nothing = Nothing
