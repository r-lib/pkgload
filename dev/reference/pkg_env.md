# Return package environment

This is an environment like `<package:pkg>`. The package environment
contains the exported objects from a package. It is attached, so it is
an ancestor of `R_GlobalEnv`.

## Usage

``` r
pkg_env(package)
```

## Arguments

- package:

  package name.

## Details

When a package is loaded the normal way, using
[`library()`](https://rdrr.io/r/base/library.html), this environment
contains only the exported objects from the namespace. However, when
loaded with
[`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md),
this environment will contain all the objects from the namespace, unless
`load_all` is used with `export_all=FALSE`.

If the package is not attached, this function returns `NULL`.

## See also

[`ns_env()`](https://pkgload.r-lib.org/dev/reference/ns_env.md) for the
namespace environment that all the objects (exported and not exported).

[`imports_env()`](https://pkgload.r-lib.org/dev/reference/imports_env.md)
for the environment that contains imported objects for the package.
