# cabal-appimage

![GiHub Actions Build Status](https://github.com/gbrsales/cabal-appimage/workflows/build/badge.svg) [![Travis CI Build Status](https://travis-ci.org/gbrsales/cabal-appimage.svg?branch=master)](https://travis-ci.org/gbrsales/cabal-appimage)

This package provides a build hook for Cabal automating the creation
of AppImage bundles.

Internally, it calls the
[appimagetool](https://github.com/AppImage/AppImageKit) and
[linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) utilities
which must be already installed on the system.
