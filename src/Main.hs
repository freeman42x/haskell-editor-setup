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
main = run $ startApp App { .. }
 where
  initialAction = NoOp
  model         = Model Atom ""
  update        = updateModel
  view          = viewModel
  events        = defaultEvents
  subs          = []
  mountPoint    = Nothing

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (SetChecked editorOrIde_ (Checked True)) m =
  noEff $ m & editorOrIde .~ editorOrIde_
updateModel (SetChecked _ _) m = noEff m
updateModel (Append appendText) model = noEff model {  _log = _log model <> appendText }
updateModel Install model = effectSub model (liftIO . nixOsAtom)
