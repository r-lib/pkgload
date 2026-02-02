# Replacement version of system.file

This function is meant to intercept calls to
[`base::system.file()`](https://rdrr.io/r/base/system.file.html), so
that it behaves well with packages loaded by devtools. It is made
available when a package is loaded with
[`load_all()`](https://pkgload.r-lib.org/reference/load_all.md).

## Usage

``` r
shim_system.file(..., package = "base", lib.loc = NULL, mustWork = FALSE)
```

## Arguments

- ...:

  character vectors, specifying subdirectory and file(s) within some
  package. The default, none, returns the root of the package. Wildcards
  are not supported.

- package:

  a character string with the name of a single package. An error occurs
  if more than one package name is given.

- lib.loc:

  a character vector with path names of R libraries. See ‘Details’ for
  the meaning of the default value of `NULL`.

- mustWork:

  logical. If `TRUE`, an error is given if there are no matching files.

## Details

When `system.file` is called from the R console (the global
environment), this function detects if the target package was loaded
with [`load_all()`](https://pkgload.r-lib.org/reference/load_all.md),
and if so, it uses a customized method of searching for the file. This
is necessary because the directory structure of a source package is
different from the directory structure of an installed package.

When a package is loaded with `load_all`, this function is also inserted
into the package's imports environment, so that calls to `system.file`
from within the package namespace will use this modified version. If
this function were not inserted into the imports environment, then the
package would end up calling
[`base::system.file`](https://rdrr.io/r/base/system.file.html) instead.
