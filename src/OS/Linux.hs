module OS.Linux where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( replace
                                                , isInfixOf
                                                , unpack
                                                , Text)
import           Data.Text.IO                   ( readFile
                                                , writeFile
                                                )
import           Miso.Effect
import           Miso.String                    ( toMisoString
                                                , MisoString)
import           NeatInterpolation
import           Prelude hiding (readFile, writeFile)

import           Types
import           OS.Common

data ExtensionInfo = ExtensionInfo MisoString Text

configure :: Model -> Sink Action -> IO ()
configure m sink = do
  let run = view runConfigure m
  configureNix run sink
  nixConfiguration <- getOptimalNixConfiguration
  configureNixConfiguration nixConfiguration run sink
  mapM_ (configureNixPackage nixConfiguration run sink) $ uncurry ExtensionInfo <$>
    [ ("GHC", "haskell.compiler.ghc865")
    , ("cabal-install", "haskellPackages.cabal-install")
    , ("Atom", "atom")
    , ("Haskell IDE Engine", "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
    \ {}).selection { selector = p: { inherit (p) ghc865; }; })") ]
  mapM_ (configureAtomPackage run sink)
    [ "nix"
    , "atom-ide-ui"
    , "autocomplete-haskell"
    , "hasklig"
    , "ide-haskell-cabal"
    , "ide-haskell-hasktags"
    , "ide-haskell-hie"
    , "ide-haskell-hoogle"
    , "ide-haskell-repl"
    , "language-haskell" ]

appendLog :: MisoString -> Sink Action -> IO ()
appendLog text sink = sink $ Append (text <> "\n")

logStep :: MisoString -> Sink Action -> IO a -> IO ()
logStep text sink actions = do
  appendLog ("BEGIN : " <> text) sink
  _ <- actions
  appendLog ("END   : " <> text) sink

configureNix :: Bool -> Sink Action -> IO ()
configureNix run sink = do
  isNixInstalled' <- isNixInstalled
  if isNixInstalled'
    then
      appendLog "Nix is already installed" sink
    else
      logStep "Installing Nix" sink (when run $ do
        output <- runShellCommand "curl -L https://nixos.org/nix/install | sh"
        appendLog (toMisoString output) sink
        nixChannelsFilePath <- toFullFilePath "~/.nix-channels"
        writeFile nixChannelsFilePath "nixpkgs https://nixos.org/channels/nixos-20.03")

configureNixConfiguration :: NixConfiguration -> Bool -> Sink Action -> IO ()
configureNixConfiguration NixOS _ _ = return ()
configureNixConfiguration User run sink = do
  configurationNixFilePath <- getNixConfigurationPath User
  configExists <- doesFileExist' configurationNixFilePath
  unless configExists
    $ logStep ("Writing default user configuration") sink
    $ when run $ writeFile configurationNixFilePath configNixContent

configNixContent :: Text
configNixContent =
  [text|
    with import <nixpkgs> {};

    {
      allowUnfree = true;

      packageOverrides = pkgs: rec {
        all = pkgs.buildEnv {
          name = "all";

          paths = [

          ];
        };
      };
    }
  |]

configureNixPackage :: NixConfiguration -> Bool -> Sink Action -> ExtensionInfo -> IO ()
configureNixPackage nixConfiguration run sink (ExtensionInfo name package) = do
  configurationNixFilePath <- getNixConfigurationPath nixConfiguration
  oldConfigurationNixText <- liftIO $ readFile configurationNixFilePath

  -- FIXME vvv requires Nix parsing using HNIX
  let environmentSystemPackages = "environment.systemPackages = with pkgs; ["
      isPackagePresent = package `isInfixOf` oldConfigurationNixText
      newConfigurationNixText =
        if isPackagePresent
          then oldConfigurationNixText
          else replace
            environmentSystemPackages
            (environmentSystemPackages <> "\n\
                \    " <> package)
            oldConfigurationNixText
  -- ^^^

  when run $ liftIO $ writeFile configurationNixFilePath newConfigurationNixText
  if oldConfigurationNixText == newConfigurationNixText -- OPTIMIZE
    then appendLog ("Nix package " <> name <> " already installed") sink
    else logStep ("Installing Nix package - " <> name) sink (when run $ do
      output <- runShellCommand "nixos-rebuild switch"
      appendLog (toMisoString output) sink)

-- TODO install or update? extension or log message
-- TODO ensure extensions are enabled if not enable them
configureAtomPackage :: Bool -> Sink Action -> Text -> IO ()
configureAtomPackage run sink package = do
  --   check if package isAtomPackageInstalled
  --   install if not installed
  --   update if installed
  let installingPackage = "Installing Atom package - " <> toMisoString package
  logStep installingPackage sink $
    when run $ installAtomPackage package