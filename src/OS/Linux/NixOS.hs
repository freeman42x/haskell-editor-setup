module OS.Linux.NixOS where

import           Control.Monad.IO.Class         ( liftIO )
import           Prelude                        ( IO
                                                , String
                                                , return
                                                , ($)
                                                , (<>)
                                                )
import           Data.Bifoldable                ( bifold )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( putStrLn
                                                , readFile
                                                , writeFile
                                                )
import           Miso.Effect
import           Miso.String                    ( toMisoString )
import           Turtle                         ( shell
                                                , sh
                                                , inshellWithErr
                                                , empty
                                                , die
                                                , repr
                                                , ExitCode(..)
                                                )
import           Turtle.Line                    ( lineToText )

import           Types

nixOsAtom :: Sink Action -> IO ()
nixOsAtom sink = do
  let log text = sink $ Append (text <> "\n")
      logStep text actions = do
        log $ "BEGIN : " <> text
        _ <- actions
        log $ "END   : " <> text
      runShellCommand command = sh $ do
        out <- inshellWithErr command empty
        liftIO $ log $ toMisoString $ lineToText $ bifold out

  logStep "Configuring Haskell GHC" $ do
    oldConfigurationNixText <- liftIO $ readFile configurationNixFile
    let newConfigurationNixText = addPackageToSystemPackagesIfItDoesNotExist
          oldConfigurationNixText
          "haskell.compiler.ghc865"
    liftIO $ writeFile configurationNixFile newConfigurationNixText

  -- TODO run only if changed or new
  logStep "Installing Haskell GHC" (runShellCommand "nixos-rebuild switch")

  -- TODO function for begin / end log blocks
  -- log "Adding Haskell GHC and cabal-install to configuration.nix"
  -- oldConfigurationNixText <- liftIO $ readFile configurationNixFile
  -- let newConfigurationNixText = foldl
  --       addPackageToSystemPackagesIfItDoesNotExist
  --       oldConfigurationNixText
  --       ["haskell.compiler.ghc865", "haskellPackages.cabal-install", "atom"]
  -- liftIO $ writeFile configurationNixFile newConfigurationNixText
  -- log "Finished adding Haskell GHC and cabal-install to configuration.nix"

  log "Installing GHC, cabal-install and Atom"
  -- exitCode <- shell "nixos-rebuild switch" empty
  -- case exitCode of
  --   ExitSuccess -> return ()
  --   ExitFailure n ->
  --     die ("nixos-rebuild switch failed with exit code: " <> repr n)
  log "Finished installing GHC, cabal-install and Atom"

  log "Adding Haskell IDE Engine to configuration.nix"
  -- config2 <- liftIO $ readFile configurationNixFile
  -- let
  --   newConfig2 = addPackageToSystemPackagesIfItDoesNotExist
  --     config2
  --     "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
  --         \ {}).selection { selector = p: { inherit (p) ghc865 ghc864; }; })"
  -- liftIO $ writeFile configurationNixFile newConfig2
  log "Finished adding Haskell IDE Engine to configuration.nix"

  log "Installing Haskell IDE Engine"
  -- exitCode2 <- shell "nixos-rebuild switch" empty
  -- case exitCode2 of
  --   ExitSuccess -> return ()
  --   ExitFailure n ->
  --     die ("nixos-rebuild switch failed with exit code: " <> repr n)
  log "Finished installing Haskell IDE Engine"

  -- liftIO $ do
  --   installAtomPackage "nix"
  --   installAtomPackage "atom-ide-ui"
  --   installAtomPackage "autocomplete-haskell"
  --   installAtomPackage "hasklig"
  --   installAtomPackage "ide-haskell-cabal"
  --   installAtomPackage "ide-haskell-hasktags"
  --   installAtomPackage "ide-haskell-hie"
  --   installAtomPackage "ide-haskell-hoogle"
  --   installAtomPackage "ide-haskell-repl"
  --   installAtomPackage "language-haskell"


putStrLnGreen :: T.Text -> IO ()
putStrLnGreen str = putStrLn $ "\x1b[32m" <> str <> "\x1b[0m"

configurationNixFile :: String
configurationNixFile = "/etc/nixos/configuration.nix"

environmentSystemPackages :: T.Text
environmentSystemPackages = "environment.systemPackages = with pkgs; ["

addPackageToSystemPackagesIfItDoesNotExist :: T.Text -> T.Text -> T.Text
addPackageToSystemPackagesIfItDoesNotExist configurationNix package =
  if isPackagePresent
    then configurationNix
    -- FIXME
    else T.replace
      environmentSystemPackages
      (environmentSystemPackages <> "\n\
           \    " <> package)
      configurationNix
  where isPackagePresent = package `T.isInfixOf` configurationNix -- TODO HACK

installAtomPackage :: T.Text -> IO ()
installAtomPackage package = do
  putStrLnGreen $ "Installing " <> package
  exitCode <- shell ("sudo -u $SUDO_USER apm install " <> package) empty
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure n -> die ("apm install failed with exit code: " <> repr n)
  putStrLnGreen $ "Finished installing " <> package
