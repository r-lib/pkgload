# Helper functions for working with development packages.

All functions search recursively up the directory tree from the input
path until they find a DESCRIPTION file.

## Usage

``` r
pkg_path(path = ".")

pkg_name(path = ".")

pkg_desc(path = ".")

pkg_version(path = ".")

pkg_version_raw(path = ".")

pkg_ns(path = ".")
```

## Arguments

- path:

  Path to a package, or within a package.

## Functions

- `pkg_path()`: Return the normalized package path.

- `pkg_name()`: Return the package name.

- `pkg_desc()`: Return the package DESCRIPTION as a
  [`desc::desc()`](https://desc.r-lib.org/reference/desc.html) object.

- `pkg_version()`: Return the parsed package version.

- `pkg_version_raw()`: Return the raw package version (as a string).

- `pkg_ns()`: Return the package namespace.
