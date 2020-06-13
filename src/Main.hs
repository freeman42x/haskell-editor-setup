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
import           Data.Text.IO                   ( putStrLn )
import           Prelude                        ( IO
                                                , Bool(..)
                                                , pure
                                                , return
                                                , ($)
                                                , (>>)
                                                )
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
  _ <- forkIO $ JSaddle.run 8080 $ startApp App { .. }
  _ <- proc "nw" ["."] empty
  return ()
 where
  initialAction = NoOp   -- initial action to be executed on application load
  model         = Model Atom    -- initial model
  update        = updateModel   -- update function
  view          = viewModel     -- view function
  events        = defaultEvents -- default delegated events
  subs          = []            -- empty subscription list
  mountPoint    = Nothing   -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (SetChecked editorOrIde_ (Checked True)) m =
  noEff $ m & editorOrIde .~ editorOrIde_
updateModel (SetChecked _ _) m = noEff m
updateModel (Append _      ) m = noEff m -- TODO
updateModel Install          m = m <# do
  liftIO runSetup >> pure NoOp
 where
  runSetup = case _editorOrIde m of
    Atom -> nixOsAtom
    _    -> putStrLn "Not implemented yet"
