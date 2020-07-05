module OS.Linux where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Text                      ( replace
                                                , isInfixOf
                                                , Text)
import           Data.Text.IO                   ( readFile
                                                , writeFile
                                                )
import           Miso.Effect
import           Miso.String                    ( toMisoString
                                                , MisoString)
import           Prelude hiding (readFile, writeFile)

import           Types
import           OS.Common

data ExtensionInfo = ExtensionInfo MisoString Text

configure :: Model -> Sink Action -> IO ()
configure m sink = do
  let run = view runConfigure m
  configureNix run sink
  mapM_ (configureNixPackage run sink) $ uncurry ExtensionInfo <$>
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
configureNix run sink = undefined

configureNixPackage :: Bool -> Sink Action -> ExtensionInfo -> IO ()
configureNixPackage run sink (ExtensionInfo name package) = do
  nixConfiguration <- getOptimalNixConfiguration
  let configurationNixFilePath = "/etc/nixos/configuration.nix"
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
      _ <- runShellCommand "nixos-rebuild switch"
      return ())

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