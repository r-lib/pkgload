# Load all of the imports for a package

The imported objects are copied to the imports environment, and are not
visible from `R_GlobalEnv`. This will automatically load (but not
attach) the dependency packages.

## Usage

``` r
load_imports(path = ".")
```
