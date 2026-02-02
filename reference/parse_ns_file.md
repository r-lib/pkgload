# Parses the NAMESPACE file for a package

Parses the NAMESPACE file for a package

## Usage

``` r
parse_ns_file(path = ".")
```

## Arguments

- path:

  Path to a package, or within a package.

## Examples

``` r
if (has_tests()) {
parse_ns_file(pkgtest("testLoadHooks"))
}
```
