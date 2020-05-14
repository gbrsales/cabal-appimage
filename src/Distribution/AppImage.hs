{-|
Module:    Distribution.AppImage
Copyright: 2020 Gabriele Sales

This bundle provides a custom build hook that automatically wraps executables
inside AppImage bundles.

Internally, it calls the @appimagetool@ and @linuxdeploy@ utilities which must
be already installed on the system.
-}

{-# LANGUAGE RecordWildCards #-}

module Distribution.AppImage
  ( AppImage(..)
  , appImageBuildHook
  )
where

import           Control.Monad
import           Data.String
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Program
import           Distribution.Simple.Program.Types
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.System
import           Distribution.Verbosity
import           System.FilePath


data AppImage = AppImage {
  -- | Application name. The AppImage bundle will be produced in
  -- @dist\/build\//appName/.AppImage@ and will contain the executable
  -- /appName/.
  appName      :: String,
  -- | Path to desktop file.
  appDesktop   :: FilePath,
  -- | Path to icon file.
  appIcon      :: FilePath,
  -- | Other files to include in the application bundle. Will be copied in
  -- the @\/usr\/share\//appName/@ directory inside the image.
  appResources :: [FilePath]
  } deriving (Eq, Show)


-- | Post-build hook for AppImage bundles. Does nothing if the OS is not Linux.
--
-- Use this function as a @postBuild@ hook.
appImageBuildHook
  :: [AppImage] -- ^ Applications to build.
  -> Args       -- ^ Other parameters as defined in 'Distribution.Simple.postBuild'.
  -> BuildFlags
  -> PackageDescription
  -> LocalBuildInfo
  -> IO ()
appImageBuildHook apps _ flags pkg buildInfo =
  when (buildOS == Linux) $
    let bdir = buildDir buildInfo
        verb = fromFlagOrDefault normal (buildVerbosity flags)
    in forM_ apps (makeBundle pkg bdir verb)

makeBundle :: PackageDescription -> FilePath -> Verbosity -> AppImage -> IO ()
makeBundle pkg bdir verb app@AppImage{..} = do
  unless (hasExecutable pkg appName) $
    die' verb ("No executable defined for the AppImage bundle: " ++ appName)
  withTempDirectory verb bdir "appimage." $ \appDir -> do
    let exe   = bdir </> appName </> appName
        share = appDir </> "usr" </> "share" </> appName
    createDirectoryIfMissingVerbose verb True share
    deployExe app appDir exe verb
    copyResources appResources share verb
    bundleApp appDir verb

hasExecutable :: PackageDescription -> String -> Bool
hasExecutable pkg name =
  any (\e -> exeName e == fromString name) (executables pkg)

deployExe :: AppImage -> FilePath -> FilePath -> Verbosity -> IO ()
deployExe AppImage{..} appDir exe verb = do
  prog <- findProg "linuxdeploy" verb
  runProgram verb prog
    [ "--appdir=" ++ appDir
    , "--executable=" ++ exe
    , "--desktop-file=" ++ appDesktop
    , "--icon-file=" ++ appIcon
    ]

copyResources :: [FilePath] -> FilePath -> Verbosity -> IO ()
copyResources resources dest verb = mapM_ copy resources
  where
    copy res = copyFileVerbose verb res (dest </> takeFileName res)

bundleApp :: FilePath -> Verbosity -> IO ()
bundleApp appDir verb = do
  prog <- findProg "appimagetool" verb
  let (wdir, name) = splitFileName appDir
  runProgramInvocation verb $
    (programInvocation prog [name]) { progInvokeCwd = Just wdir }

findProg :: String -> Verbosity -> IO ConfiguredProgram
findProg name verb = do
  found <- findProgramOnSearchPath verb defaultProgramSearchPath name
  case found of
    Nothing        -> die' verb ("Command " ++ name ++ " is not available")
    Just (path, _) -> return (simpleConfiguredProgram name (FoundOnSystem path))
