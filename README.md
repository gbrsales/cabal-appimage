# cabal-appimage

This package provides a build hook for Cabal automating the creation
of AppImage bundles.

Internally, it calls the
[appimagetool](https://github.com/AppImage/AppImageKit) and
[linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) utilities
which must be already installed on the system.
