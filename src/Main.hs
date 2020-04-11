module Main where

import           Prelude                        ( IO
                                                , Show
                                                , Eq
                                                , Char
                                                , Maybe(..)
                                                , ($)
                                                , (<>)
                                                , getChar
                                                , return
                                                , lookup
                                                , show
                                                )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( putStrLn )
import           Turtle                         ( hostname )

import           OS.Linux.NixOS                 ( nixOsAtom )
import           OS.Linux.Debian                ( debianAtom )


data EditorOrIde =
    Atom
  | VisualStudioCode
  | IntelliJIdeaCommunity
  | SublimeText3
  | Leksah
  deriving (Show, Eq)

charToEditorOrIde :: Char -> Maybe EditorOrIde
charToEditorOrIde char = case char of
  '1' -> Just Atom
  '2' -> Just VisualStudioCode
  '3' -> Just IntelliJIdeaCommunity
  '4' -> Just SublimeText3
  '5' -> Just Leksah
  _   -> Nothing

askEditorOrIde :: IO EditorOrIde
askEditorOrIde = do
  putStrLn $ T.intercalate
    "\n"
    [ "Choose editor/IDE"
    , "(1) Atom"
    , "(2) VSCode "
    , "(3) IntelliJ IDEA"
    , "(4) Sublime Text 3"
    , "(5) Leksah"
    ]
  prompt
 where
  prompt = do
    putStrLn "Your choice: "
    choice <- getChar
    case charToEditorOrIde choice of
      Just editorOrIde -> return editorOrIde
      _                -> do
        putStrLn "Wrong choice, please try again"
        prompt

-- TODO: come up with more meaningful name
automationDB :: [(EditorOrIde, [(T.Text, IO ())])]
automationDB = [(Atom, [("nixos", nixOsAtom), ("debian", debianAtom)])]

main :: IO ()
main = do
  editorOrIde <- askEditorOrIde
  os          <- hostname

  let textEditorOrIde = T.pack $ show editorOrIde

  case lookup editorOrIde automationDB of
    -- TODO: come up with more meaningful name
    Just osFuncDB -> case lookup os osFuncDB of
      Just installation -> installation
      _ ->
        putStrLn $ textEditorOrIde <> " on " <> os <> " is not supported yet"
    _ -> putStrLn $ textEditorOrIde <> " is not supported yet"