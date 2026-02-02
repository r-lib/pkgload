# Return imports environment for a package

Contains objects imported from other packages. Is the parent of the
package namespace environment, and is a child of `<namespace:base>`,
which is a child of `R_GlobalEnv`.

## Usage

``` r
imports_env(package)
```

## Arguments

- package:

  The package name as a string.

## See also

[`ns_env()`](https://pkgload.r-lib.org/reference/ns_env.md) for the
namespace environment that all the objects (exported and not exported).

[`pkg_env()`](https://pkgload.r-lib.org/reference/pkg_env.md) for the
attached environment that contains the exported objects.
