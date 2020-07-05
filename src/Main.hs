module Main where

import           Control.Lens                   ( (.~)
                                                , (&)
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Concurrent             ( forkIO )
import           Miso
import           Language.Javascript.JSaddle.Warp
                                               as JSaddle
import           Data.Maybe                     ( Maybe(..) )
import           Prelude
import           Turtle                         ( proc
                                                , empty
                                                )

import           OS.Linux
import           Types
import           View

main :: IO ()
main = do
  -- TODO: can leak resources if JSaddle does something special
  -- better to use @Control.Concurrent.Async.race@ maybe?
  _ <- forkIO $ JSaddle.run 8080 $ startApp App {
    initialAction = NoOp,        -- initial action to be executed on application load
    model  = Model Atom False "", -- initial model
    update = updateModel,        -- update function
    view   = viewModel,          -- view function
    events = defaultEvents,      -- default delegated events
    subs   = [],                 -- empty subscription list
    mountPoint = Nothing         -- mount point for application (Nothing defaults to 'body')
  }
  _ <- proc "nw" ["."] empty
  return ()

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (SetEditorOrIde editorOrIde_ (Checked True)) m = noEff $ m & editorOrIde .~ editorOrIde_
updateModel (SetEditorOrIde _ _) m = noEff m
updateModel (SetRun (Checked checked)) m = noEff $ m & runConfigure .~ checked
updateModel (Append appendText) m = noEff m {  _log = _log m <> appendText }
updateModel Install m = effectSub m (liftIO . configure m)