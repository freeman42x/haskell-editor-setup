module Types where

import           Miso
import qualified Miso.String                   as MS
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

data Action
  = NoOp
  | SetChecked EditorOrIde Checked
  | Install
  | Append MS.MisoString
  deriving (Show, Eq)