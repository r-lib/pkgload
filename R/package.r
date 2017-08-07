#' Find file in a package.
#'
#' It always starts by finding by walking up the path until it finds the
#' root directory, i.e. a directory containing `DESCRIPTION`. If it
#' cannot find the root directory, or it can't find the specified path, it
#' will throw an error.
#'
#' @param ... Components of the path.
#' @param path Place to start search for package directory.
#' @export
#' @examples
#' \dontrun{
#' package_file("figures", "figure_1")
#' }
package_file <- function(..., path = ".") {
  file.path(pkg_path(path), ...)
}

# Mockable variant of interactive
interactive <- function() .Primitive("interactive")()

#' Is the package currently under development?
#'
#' Returns `TRUE` or `FALSE` depending on if the package has been loaded by
#' **pkgload**.
#'
#' @param name the name of a package.
#' @export
is_dev_package <- function(name) name %in% dev_packages()

pkg_path <- function(path = ".") {
  rprojroot::find_root("DESCRIPTION", path)
}

pkg_name <- function(path = ".") {
  desc::desc_get("Package", pkg_path(path))[[1]]
}

pkg_desc <- function(path = ".") {
  desc::desc(pkg_path(path))
}

pkg_version <- function(path = ".") {
  desc::desc_get_version(pkg_path(path))
}
