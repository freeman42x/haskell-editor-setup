module Types where

import           Control.Lens                   ( makeLenses )
import           Miso
import qualified Miso.String                   as MS

data EditorOrIde =
    Atom
  | VisualStudioCode
  | IntelliJIdeaCommunity
  | SublimeText3
  | Leksah
  deriving (Show, Eq)

data Action
  = NoOp
  | SetEditorOrIde EditorOrIde Checked
  | SetRun Checked
  | Install
  | Append MS.MisoString
  deriving (Show, Eq)

data Model = Model
  { _editorOrIde :: EditorOrIde,
    _runConfigure :: Bool,
    _log :: MS.MisoString
  } deriving (Show, Eq)

makeLenses ''Model