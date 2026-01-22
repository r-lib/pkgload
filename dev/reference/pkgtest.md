# Return the path to one of the packages in the devtools test dir

devtools comes with some simple packages for testing. This function
returns the path to them.

## Usage

``` r
pkgtest(package)
```

## Arguments

- package:

  Name of the test package.

## Examples

``` r
if (has_tests()) {
pkgtest("testData")
}
```
