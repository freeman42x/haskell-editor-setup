-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String
import Language.Javascript.JSaddle.Warp as JSaddle
import Control.Monad.IO.Class (liftIO)

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = NoOp
  | Install
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = JSaddle.run 8080 $ startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel Install m = m <# do
  liftIO (putStrLn "Hello World") >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = form_ [] [
   link_ [ rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css" ]
 , h5_ [ class_ "title is-5" ] [ text "Easy Haskell Editor / IDE Setup" ]
 , div_ [ class_ "control" ] [
     "Editor / IDE"
   , br_ []
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", value_ "Atom" ]
       , "Atom"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", value_ "VSCode", disabled_ True ]
       , "Visual Studio Code"
     ]
 ]
 , br_ []
 , text (ms x)
 , button_ [ onClick Install, class_ "button" ] [ text "Install" ]
 ]
