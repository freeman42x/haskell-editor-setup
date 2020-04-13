module Types where

import qualified Data.Text                     as T
import           Miso
import           Prelude                        ( Show
                                                , Eq
                                                )



data EditorOrIde =
    Atom
  | VisualStudioCode
  | IntelliJIdeaCommunity
  | SublimeText3
  | Leksah
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = NoOp
  | SetChecked EditorOrIde Checked
  | Install
  | Append T.Text
  deriving (Show, Eq)