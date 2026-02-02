# Parse package dependency strings.

Parse package dependency strings.

## Usage

``` r
parse_deps(string)
```

## Arguments

- string:

  to parse. Should look like `"R (>= 3.0), ggplot2"` etc.

## Value

list of two character vectors: `name` package names, and `version`
package versions. If version is not specified, it will be stored as NA.

## Examples

``` r
parse_deps("httr (< 2.1),\nRCurl (>= 3)")
#>    name compare version
#> 1  httr       <     2.1
#> 2 RCurl      >=       3
# only package dependencies are returned
parse_deps("utils (== 2.12.1),\ntools,\nR (>= 2.10),\nmemoise")
#>      name compare version
#> 1   utils      ==  2.12.1
#> 2   tools    <NA>    <NA>
#> 4 memoise    <NA>    <NA>
```
