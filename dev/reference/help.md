# Drop-in replacements for help and ? functions

The `?` and `help` functions are replacements for functions of the same
name in the utils package. They are made available when a package is
loaded with
[`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md).

## Usage

``` r
# help(topic, package = NULL, ...)

# ?e2
# e1?e2
```

## Arguments

- topic:

  A name or character string specifying the help topic.

- package:

  A name or character string specifying the package in which to search
  for the help topic. If NULL, search all packages.

- ...:

  Additional arguments to pass to
  [`utils::help()`](https://rdrr.io/r/utils/help.html).

- e1:

  First argument to pass along to `utils::`?“.

- e2:

  Second argument to pass along to `utils::`?“.

## Details

The `?` function is a replacement for
[`utils::?()`](https://rdrr.io/r/utils/Question.html) from the utils
package. It will search for help in devtools-loaded packages first, then
in regular packages.

The `help` function is a replacement for
[`utils::help()`](https://rdrr.io/r/utils/help.html) from the utils
package. If `package` is not specified, it will search for help in
devtools-loaded packages first, then in regular packages. If `package`
is specified, then it will search for help in devtools-loaded packages
or regular packages, as appropriate.

## Examples

``` r
if (FALSE) { # \dontrun{
# This would load devtools and look at the help for load_all, if currently
# in the devtools source directory.
load_all()
?load_all
help("load_all")
} # }

# To see the help pages for utils::help and utils::`?`:
help("help", "utils")
help("?", "utils")

if (FALSE) { # \dontrun{
# Examples demonstrating the multiple ways of supplying arguments
# NB: you can't do pkg <- "ggplot2"; help("ggplot2", pkg)
help(lm)
help(lm, stats)
help(lm, 'stats')
help('lm')
help('lm', stats)
help('lm', 'stats')
help(package = stats)
help(package = 'stats')
topic <- "lm"
help(topic)
help(topic, stats)
help(topic, 'stats')
} # }
```
