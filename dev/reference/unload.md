# Unload a package

`unload()` attempts to cleanly unload a package, including unloading its
namespace, deleting S4 class definitions and unloading any loaded DLLs.
Unfortunately S4 classes are not really designed to be cleanly unloaded,
and so we have to manually modify the class dependency graph in order
for it to work - this works on the cases for which we have tested but
there may be others. Similarly, automated DLL unloading is best tested
for simple scenarios (particularly with `useDynLib(pkgname)` and may
fail in other cases. If you do encounter a failure, please file a bug
report at <https://github.com/r-lib/pkgload/issues>.

`unregister()` is a gentler version of `unload()` which removes the
package from the search path, unregisters methods, and unregisters the
namespace. It doesn't unload the namespace or its DLL to keep it in
working order in case of dangling references.

## Usage

``` r
unload(package = pkg_name(), quiet = FALSE)

unregister(package = pkg_name())
```

## Arguments

- package:

  package name.

- quiet:

  if `TRUE` suppresses output from this function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Unload package that is in current directory
unload()

# Unload package that is in ./ggplot2/
unload(pkg_name("ggplot2/"))

library(ggplot2)
# unload the ggplot2 package directly by name
unload("ggplot2")
} # }
```
