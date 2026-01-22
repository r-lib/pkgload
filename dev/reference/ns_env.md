# Return the namespace environment for a package.

Contains all (exported and non-exported) objects, and is a descendant of
`R_GlobalEnv`. The hierarchy is `<namespace:pkg>`, `<imports:pkg>`,
`<namespace:base>`, and then `R_GlobalEnv`.

## Usage

``` r
ns_env(package)
```

## Arguments

- package:

  package name.

## Details

If the package is not loaded, this function returns `NULL`.

## See also

[`pkg_env()`](https://pkgload.r-lib.org/dev/reference/pkg_env.md) for
the attached environment that contains the exported objects.

[`imports_env()`](https://pkgload.r-lib.org/dev/reference/imports_env.md)
for the environment that contains imported objects for the package.
