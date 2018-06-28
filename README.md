# pkgload

[![Travis-CI Build Status](https://travis-ci.org/r-lib/pkgload.svg?branch=master)](https://travis-ci.org/r-lib/pkgload)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/r-lib/pkgload?branch=master&svg=true)](https://ci.appveyor.com/project/r-lib/pkgload)
[![codecov](https://codecov.io/gh/r-lib/pkgload/branch/master/graph/badge.svg)](https://codecov.io/gh/r-lib/pkgload)

The goal of pkgload is to simulate the process of installing and loading a
package, without actually doing the complete process, and hence making package
iteration much faster. This was previously part of devtools (it was in fact the
original motivation) but has been moved into its own package as part of the
devtools diaspora into smaller, more focussed packages.

## Usage

In most cases you will not use pkgload directly, and instead you'll call it via `devtools::load_all()`.

``` r
devtools::load_all()
```
