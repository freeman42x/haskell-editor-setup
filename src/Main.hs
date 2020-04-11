--{-# LANGUAGE TemplateHaskell #-}

module Main where

-- | Miso framework import
-- import           Control.Lens                   ( (.~)
--                                                 , (&)
--                                                 , makeLenses
--                                                 )
-- import           Control.Monad.IO.Class         ( liftIO )
-- import           Miso
-- import           Language.Javascript.JSaddle.WebKitGTK
--                                                 ( run )

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



-- | Type synonym for an application model
-- newtype Model = Model
--   { _editorOrIde :: EditorOrIde
--   } deriving (Show, Eq)

-- makeLenses ''Model

-- | Sum type for application events
-- data Action
--   = NoOp
--   | SetChecked EditorOrIde Checked
--   | Install
--   deriving (Show, Eq)

-- | Entry point for a miso application

-- main = run $ startApp App { .. }
--  where
--   initialAction = NoOp -- initial action to be executed on application load
--   model         = Model Atom           -- initial model
--   update        = updateModel          -- update function
--   view          = viewModel            -- view function
--   events        = defaultEvents        -- default delegated events
--   subs          = []                   -- empty subscription list
--   mountPoint    = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
-- updateModel :: Action -> Model -> Effect Action Model
-- updateModel NoOp m = noEff m
-- updateModel (SetChecked editorOrIde_ (Checked True)) m =
--   noEff $ m & editorOrIde .~ editorOrIde_
-- updateModel (SetChecked _ _) m = noEff m
-- updateModel Install          m = m <# do
--   liftIO runSetup >> pure NoOp
--  where
--   runSetup = case _editorOrIde m of
--     Atom -> nixOsAtom
--     _    -> putStrLn "Not implemented yet"

-- clickHandler :: action -> Attribute action
-- clickHandler action =
--   onWithOptions (defaultOptions { preventDefault = True }) "click" emptyDecoder
--     $ \() -> action

-- | Constructs a virtual DOM from a model
-- viewModel :: Model -> View Action
-- viewModel m = form_ [] [
--    link_ [ rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css" ]
--  , h5_ [ class_ "title is-5" ] [ text "Easy Haskell Editor / IDE Setup" ]
--  , div_ [ class_ "control" ] [
--      "Editor / IDE"
--    , br_ []
--    , label_ [ class_ "radio" ] [
--          input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == Atom), onChecked (SetChecked Atom) ]
--        , "Atom"
--      ]
--    , label_ [ class_ "radio" ] [
--          input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == VisualStudioCode), onChecked (SetChecked VisualStudioCode) ]
--        , "Visual Studio Code"
--      ]
--    , label_ [ class_ "radio" ] [
--          input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == IntelliJIdeaCommunity), onChecked (SetChecked IntelliJIdeaCommunity), disabled_ True ]
--        , "IntelliJ IDEA Community"
--      ]
--    , label_ [ class_ "radio" ] [
--          input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == SublimeText3), onChecked (SetChecked SublimeText3), disabled_ True ]
--        , "Sublime Text 3"
--      ]
--    , label_ [ class_ "radio" ] [
--          input_ [ type_ "radio", name_ "editor", checked_ (_editorOrIde m == Leksah), onChecked (SetChecked Leksah), disabled_ True ]
--        , "Leksah"
--      ]
--  ]
--  , br_ []
--  , button_ [ clickHandler Install , class_ "button" ] [ text "Install" ]
--  ]
