# Load R code.

Sources all `.R`/`.r` files in the `R/` directory, storing results into
the package namespace.

## Usage

``` r
load_code(path = ".", quiet = NULL)
```

## Arguments

- path:

  Path to a package, or within a package.

- quiet:

  if `TRUE` suppresses output from this function.
