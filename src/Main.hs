-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Miso
import Miso.String
import Language.Javascript.JSaddle.Warp as JSaddle

-- | Type synonym for an application model
data Model = Model
  { _editorOrIde :: EditorOrIde
  } deriving (Show, Eq)

data EditorOrIde =
    Atom
  | VisualStudioCode
  | IntelliJIdeaCommunity
  | SublimeText3
  | Leksah
  deriving (Show, Eq)

makeLenses ''Model

-- | Sum type for application events
data Action
  = NoOp
  | SetChecked EditorOrIde Checked
  | Install
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = JSaddle.run 8080 $ startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = Model Atom           -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (SetChecked editorOrIde_ (Checked True)) m = noEff $ m & editorOrIde +~ editorOrIde_
updateModel (SetChecked _ _) m = noEff m
updateModel Install m = m <# do
  liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = form_ [] [
   link_ [ rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css" ]
 , h5_ [ class_ "title is-5" ] [ text "Easy Haskell Editor / IDE Setup" ]
 , div_ [ class_ "control" ] [
     "Editor / IDE"
   , br_ []
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", onChecked (SetChecked Atom) ]
       , "Atom"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", onChecked (SetChecked VisualStudioCode) ]
       , "Visual Studio Code"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", onChecked (SetChecked IntelliJIdeaCommunity), disabled_ True ]
       , "IntelliJ IDEA Community"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", onChecked (SetChecked SublimeText3), disabled_ True ]
       , "Sublime Text 3"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", onChecked (SetChecked Leksah), disabled_ True ]
       , "Leksah"
     ]
 ]
 , br_ []
 , button_ [ onClick Install, class_ "button" ] [ text "Install" ]
 ]
