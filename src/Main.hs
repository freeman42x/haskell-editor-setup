module Main where

import Control.Lens ((.~), (&))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Miso
import Language.Javascript.JSaddle.Warp as JSaddle
import Control.Monad (unless)
import Data.Maybe (isNothing, Maybe(..))
import Data.Text (Text, replace, isInfixOf)
import Data.Text.IO (putStrLn, readFile, writeFile)
import Prelude (IO, String, Show, Eq, Bool(..), foldl', pure, return, ($), (<>), (==), (>>))
import System.Directory (findExecutable)
import Turtle (proc, shell, empty, die, repr, ExitCode(..))

import OS.Linux
import Types

main :: IO ()
main = do
  -- TODO: can leak resources if JSaddle does something special
  -- better to use @Control.Concurrent.Async.race@ maybe?
  _ <- forkIO $ JSaddle.run 8080 $ startApp App {..}
  _ <- proc "nw" ["."] empty
  return ()
  where
    initialAction = NoOp   -- initial action to be executed on application load
    model  = Model Atom    -- initial model
    update = updateModel   -- update function
    view   = viewModel     -- view function
    events = defaultEvents -- default delegated events
    subs   = []            -- empty subscription list
    mountPoint = Nothing   -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (SetChecked editorOrIde_ (Checked True)) m = noEff $ m & editorOrIde .~ editorOrIde_
updateModel (SetChecked _ _) m = noEff m
updateModel Install m = m <# do
  liftIO runSetup >> pure NoOp
  where
    runSetup = case _editorOrIde m of
                 Atom -> nixOsAtom
                 _    -> putStrLn "Not implemented yet"

clickHandler :: action -> Attribute action
clickHandler action = onWithOptions (defaultOptions { preventDefault = True }) "click" emptyDecoder $ \() -> action

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = form_ [] [
   link_ [ rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css" ]
 , h5_ [ class_ "title is-5" ] [ text "Easy Haskell Editor / IDE Setup" ]
 , div_ [ class_ "control" ] [
     "Editor / IDE"
   , br_ []
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == Atom), onChecked (SetChecked Atom) ]
       , "Atom"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == VisualStudioCode), onChecked (SetChecked VisualStudioCode) ]
       , "Visual Studio Code"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == IntelliJIdeaCommunity), onChecked (SetChecked IntelliJIdeaCommunity), disabled_ True ]
       , "IntelliJ IDEA Community"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == SublimeText3), onChecked (SetChecked SublimeText3), disabled_ True ]
       , "Sublime Text 3"
     ]
   , label_ [ class_ "radio" ] [
         input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == Leksah), onChecked (SetChecked Leksah), disabled_ True ]
       , "Leksah"
     ]
 ]
 , br_ []
 , button_ [ clickHandler Install , class_ "button" ] [ text "Install" ]
 ]
