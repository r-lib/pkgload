# Get the installation path of a package

Given the name of a package, this returns a path to the installed copy
of the package, which can be passed to other devtools functions.

## Usage

``` r
inst(name)
```

## Arguments

- name:

  the name of a package.

## Details

It searches for the package in
[`.libPaths()`](https://rdrr.io/r/base/libPaths.html). If multiple dirs
are found, it will return the first one.

## Examples

``` r
inst("pkgload")
#> [1] "/home/runner/work/_temp/Library/pkgload"
inst("grid")
#> [1] "/opt/R/4.5.2/lib/R/library/grid"
```
