# Return devtools metadata environment

If the package was not loaded with devtools, returns `NULL`.

## Usage

``` r
dev_meta(name)
```

## Arguments

- name:

  The name of a loaded package

## Examples

``` r
dev_meta("stats") # NULL
#> NULL

if (has_tests()) {
# Load the test package in directory "testLoadHooks"
load_all(pkgtest("testLoadHooks"))

# Get metadata for the package
x <- dev_meta("testLoadHooks")
as.list(x)

# Clean up.
unload("testLoadHooks")
}
```
