{-|
Module:    Distribution.AppImage
Copyright: 2020 Gabriele Sales

This bundle provides a custom build hook that automatically wraps executables
inside AppImage bundles.

Internally, it calls the @appimagetool@ and @linuxdeploy@ utilities which must
be already installed on the system.
-}

module Distribution.AppImage
  ( AppImage(..)
  , appImageBuildHook
  )
where

import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup


data AppImage = AppImage {
  -- | Application name. The AppImage bundle will be produced in
  -- @dist\/build\//appName/.AppImage@ and will contain the executable
  -- /appName/.
  appName :: String
  } deriving (Eq, Show)


appImageBuildHook
  :: [AppImage] -- ^ Applications to build.
  -> Args       -- ^ Other parameters as defined in 'Distribution.Simple.postBuild'.
  -> BuildFlags
  -> PackageDescription
  -> LocalBuildInfo
  -> IO ()
appImageBuildHook = undefined
