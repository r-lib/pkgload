# Run a examples for an in-development function.

`dev_example` is a replacement for `example`. `run_example` is a
low-level function that takes a path to an Rd file.

## Usage

``` r
dev_example(topic, quiet = FALSE)

run_example(
  path,
  run_donttest = FALSE,
  run_dontrun = FALSE,
  env = new.env(parent = globalenv()),
  quiet = FALSE,
  macros = NULL,
  run,
  test
)
```

## Arguments

- topic:

  Name or topic (or name of Rd) file to run examples for

- quiet:

  If `TRUE`, does not echo code to console.

- path:

  Path to `.Rd` file

- run_donttest:

  if `TRUE`, do run `\donttest` sections in the Rd files.

- run_dontrun:

  if `TRUE`, do run `\dontrun` sections in the Rd files.

- env:

  Environment in which code will be run.

- macros:

  Custom macros to use to parse the `.Rd` file. See the `macros`
  argument of
  [`tools::parse_Rd()`](https://rdrr.io/r/tools/parse_Rd.html). If
  `NULL`, then the
  [`tools::Rd2ex()`](https://rdrr.io/r/tools/Rd2HTML.html) (and
  [`tools::parse_Rd()`](https://rdrr.io/r/tools/parse_Rd.html)) default
  is used.

- run, test:

  Deprecated, see `run_dontrun` and `run_donttest` above.

## Examples

``` r
if (FALSE) { # \dontrun{
# Runs installed example:
library("ggplot2")
example("ggplot")

# Runs development example:
dev_example("ggplot")
} # }
```
