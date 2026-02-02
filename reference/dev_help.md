# In-development help for package loaded with devtools

`dev_help()` searches for source documentation provided in packages
loaded by devtools. To improve performance, the `.Rd` files are parsed
to create to index once, then cached. Use `dev_topic_index_reset()` to
clear that index. You can manually retrieve the index for a local
package with `dev_topic_index()`.

## Usage

``` r
dev_help(
  topic,
  dev_packages = NULL,
  stage = "render",
  type = getOption("help_type")
)

dev_topic_find(topic, dev_packages = NULL)

dev_topic_index(path = ".")

dev_topic_index_reset(pkg_name)
```

## Arguments

- topic:

  name of help to search for.

- dev_packages:

  A character vector of package names to search within. If `NULL`,
  defaults to all packages loaded by devtools.

- stage:

  at which stage ("build", "install", or "render") should `\\Sexpr`
  macros be executed? This is only important if you're using `\\Sexpr`
  macro's in your Rd files.

- type:

  of html to produce: `"html"` or `"text"`. Defaults to your default
  documentation type.

- path:

  Path to package.

- pkg_name:

  Name of package.

## Examples

``` r
if (FALSE) { # \dontrun{
library("ggplot2")
help("ggplot") # loads installed documentation for ggplot

load_all("ggplot2")
dev_help("ggplot") # loads development documentation for ggplot
} # }
```
