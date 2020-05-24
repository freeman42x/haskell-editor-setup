-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Control.Lens ((.~), (&), makeLenses)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Miso
import Language.Javascript.JSaddle.Warp as JSaddle

import Control.Monad (unless)
import Data.Maybe (isNothing, Maybe(..))
import Data.Text (Text, replace, isInfixOf)
import Data.Text.IO (putStrLn, readFile, writeFile)
import Prelude (IO, String, Show, Eq, Bool(..), pure, foldl, return, userError, ioError, show, ($), (<>), (==), (>>))
import System.Directory (findExecutable)
import Turtle (proc, shell, empty, die, repr, ExitCode(..))

-- | Type synonym for an application model
newtype Model = Model
  { _editorOrIde :: EditorOrIde
  } deriving (Show, Eq)

data EditorOrIde =
    Atom
  | VisualStudioCode
  | IntelliJIdeaCommunity
  | SublimeText3
  | Leksah
  deriving (Show, Eq)

makeLenses ''Model

-- | Sum type for application events
data Action
  = NoOp
  | SetChecked EditorOrIde Checked
  | Install
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = do
  -- TODO: can leak resources if JSaddle does something special
  -- better to use @Control.Concurrent.Async.race@ maybe?
  _ <- forkIO $ JSaddle.run 8080 $ startApp App {..}
  nwExitStatus <- proc "nw" ["."] empty
  case nwExitStatus of
    ExitSuccess -> return ()
    (ExitFailure n) -> ioError $ userError $ "NW app got exit code " <> show n
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = Model Atom           -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

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



nixOsAtom :: IO ()
nixOsAtom = do
  maybeFilePath <- findExecutable "nixos-version"
  putStrLnGreen $ case maybeFilePath of
    Just _ -> "NixOS operating system detected"
    _      -> "NixOS operating system not found"
  unless (isNothing maybeFilePath) $ do
    putStrLnGreen "Adding Haskell GHC and cabal-install to configuration.nix"
    config <- readFile configurationNix
    let newConfig = foldl addToConfigurationIfDoesNotExist config ["haskell.compiler.ghc865", "haskellPackages.cabal-install", "atom"]
    writeFile configurationNix newConfig
    putStrLnGreen "Finished adding Haskell GHC and cabal-install to configuration.nix"

    putStrLnGreen "Installing GHC, cabal-install and Atom"
    exitCode <- shell "nixos-rebuild switch" empty
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure n -> die ("nixos-rebuild switch failed with exit code: " <> repr n)
    putStrLnGreen "Finished installing GHC, cabal-install and Atom"

    putStrLnGreen "Adding Haskell IDE Engine to configuration.nix"
    config2 <- readFile configurationNix
    let newConfig2 =
          addToConfigurationIfDoesNotExist
            config2
            "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
            \ {}).selection { selector = p: { inherit (p) ghc865 ghc864; }; })"
    writeFile configurationNix newConfig2
    putStrLnGreen "Finished adding Haskell IDE Engine to configuration.nix"

    putStrLnGreen "Installing Haskell IDE Engine"
    exitCode2 <- shell "nixos-rebuild switch" empty
    case exitCode2 of
        ExitSuccess   -> return ()
        ExitFailure n -> die ("nixos-rebuild switch failed with exit code: " <> repr n)
    putStrLnGreen "Finished installing Haskell IDE Engine"

    installAtomPackage "nix"
    installAtomPackage "atom-ide-ui"
    installAtomPackage "autocomplete-haskell"
    installAtomPackage "hasklig"
    installAtomPackage "ide-haskell-cabal"
    installAtomPackage "ide-haskell-hasktags"
    installAtomPackage "ide-haskell-hie"
    installAtomPackage "ide-haskell-hoogle"
    installAtomPackage "ide-haskell-repl"
    installAtomPackage "language-haskell"



putStrLnGreen :: Text -> IO ()
putStrLnGreen str = putStrLn $ "\x1b[32m" <> str <> "\x1b[0m"

configurationNix :: String
configurationNix = "/etc/nixos/configuration.nix"

environmentSystemPackages :: Text
environmentSystemPackages = "environment.systemPackages = with pkgs; ["

addToConfigurationIfDoesNotExist :: Text -> Text -> Text
addToConfigurationIfDoesNotExist configNix package =
  if isPackageInstalled then configNix else
       replace
         environmentSystemPackages
         (environmentSystemPackages <> "\n\
         \    " <> package)
         configNix
  where
    isPackageInstalled = package `isInfixOf` configNix

installAtomPackage :: Text -> IO ()
installAtomPackage package = do
  putStrLnGreen $ "Installing " <> package
  exitCode <- shell ("sudo -u $SUDO_USER apm install " <> package) empty
  case exitCode of
      ExitSuccess   -> return ()
      ExitFailure n -> die ("apm install failed with exit code: " <> repr n)
  putStrLnGreen $ "Finished installing " <> package
