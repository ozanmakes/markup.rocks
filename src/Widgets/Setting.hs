{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Widgets.Setting where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Ref
import           Data.Default
import           Data.Dependent.Map          (DSum (..))
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Monoid                 ((<>))
import           GHCJS.DOM
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.HTMLInputElement
import           GHCJS.DOM.HTMLLabelElement
import           GHCJS.DOM.HTMLSelectElement
import           GHCJS.DOM.Node              (nodeAppendChild)
import           GHCJS.DOM.Types             (Element (..))
import           GHCJS.Foreign
import           GHCJS.Types
import           Reflex
import           Reflex
import           Reflex.Dom
import           Reflex.Dom
import           Reflex.Host.Class

import           Formats
import           LocalStorage
import           Widgets.Misc                (icon)

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(makeCheckbox, "jQuery($1)['checkbox']()", HTMLElement -> IO ())
JS(makeDropdown, "dropdownOnChange($1, $2)", HTMLElement -> JSFun (JSString -> IO ()) -> IO ())
JS(dropdownSetValue,"jQuery($1)['dropdown']('set text', $2)", HTMLElement -> JSString -> IO ())

data Setting t =
  Setting {_setting_value :: Dynamic t Bool}

data Selection t =
  Selection {_selection_value :: Dynamic t String
            ,_selection_setValue :: Event t String}

data SelectionConfig t =
  SelectionConfig {_selectionConfig_initialValue :: String
                  ,_selectionConfig_label :: String
                  ,_selectionConfig_options :: Dynamic t (Map String String)
                  ,_selectionConfig_setValue :: Event t String}

instance Reflex t => Default (SelectionConfig t) where
  def =
    SelectionConfig {_selectionConfig_initialValue = ""
                    ,_selectionConfig_label = ""
                    ,_selectionConfig_options = constDyn mempty
                    ,_selectionConfig_setValue = never}

setting :: MonadWidget t m => String -> Bool -> m (Setting t)
setting labelText initial =
  do val <- liftIO (getPref labelText initial)
     (parent,(input,_)) <-
       elAttr' "div" ("class" =: "ui toggle checkbox") $
       do el "label" (text labelText)
          elAttr' "input"
                  ("type" =: "checkbox" <>
                   if val
                      then "checked" =: "checked"
                      else mempty) $
            return ()
     liftIO (makeCheckbox $ _el_element parent)
     eClick <-
       wrapDomEvent (_el_element parent)
                    elementOnclick $
       liftIO $
       do checked <-
            htmlInputElementGetChecked
              (castToHTMLInputElement $ _el_element input)
          setPref labelText $ show checked
          return checked
     dValue <- holdDyn val eClick
     return (Setting dValue)

selection :: MonadWidget t m
          => SelectionConfig t -> m (Selection t)
selection (SelectionConfig k0 labelText options eSet) =
  do (eRaw,_) <-
       elAttr' "div" ("class" =: "ui dropdown compact search button") $
       do elClass "span" "text" (text labelText)
          icon "dropdown"
          divClass "menu" $
            do optionsWithDefault <-
                 mapDyn (`Map.union` (k0 =: "")) options
               listWithKey optionsWithDefault $
                 \k v ->
                   elAttr "div"
                          ("data-value" =: k <> "class" =: "item")
                          (dynText v)
     postGui <- askPostGui
     runWithActions <- askRunWithActions
     (eRecv,eRecvTriggerRef) <- newEventWithTriggerRef
     onChangeFun <-
       liftIO $
       syncCallback1
         AlwaysRetain
         True
         (\kStr ->
            do let val = fromJSString kStr
               maybe (return ())
                     (\t ->
                        postGui $
                        runWithActions
                          [t :=>
                           Just val]) =<<
                 readRef eRecvTriggerRef)
     liftIO $
       makeDropdown (_el_element eRaw)
                    onChangeFun
     let readKey opts mk =
           fromMaybe k0 $
           do k <- mk
              guard $
                Map.member k opts
              return k
     performEvent_ $
       fmap (liftIO .
             dropdownSetValue (_el_element eRaw) .
             toJSString .
             flip (Map.findWithDefault "md") sourceFormats .
             extToSource)
            eSet
     dValue <-
       combineDyn readKey options =<<
       holdDyn (Just k0)
               (leftmost [fmap Just eSet,eRecv])
     return (Selection dValue never)
