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

#' Helper functions for working with development packages.
#'
#' All functions search recursively up the directory tree from the input path
#' until they find a DESCRIPTION file.
#' @inheritParams load_all
#' @name packages
NULL

#' @describeIn packages Return the normalized package path.
#' @export
pkg_path <- function(path = ".") {
  path <- tryCatch(
    {
      rprojroot_find_package_root_file(path = path)
    },
    error = function(e) {
      msg <- c(
        "Could not find a root 'DESCRIPTION' file that starts with '^Package' in {.path {normalizePath(path)}}.",
        "i" = "Are you in your project directory and does your project have a 'DESCRIPTION' file?"
      )
      cli::cli_abort(msg, class = "pkgload_no_desc")
    }
  )

  # Strip trailing slashes, which can cause errors on Windows (#73)
  sub("[/\\]$", "", path)
}

#' @describeIn packages Return the package name.
#' @export
pkg_name <- function(path = ".") {
  desc_desc_get_field("Package", file = pkg_path(path))
}

#' @describeIn packages Return the package DESCRIPTION as a [desc::desc()] object.
#' @export
pkg_desc <- function(path = ".") {
  desc_desc(pkg_path(path))
}

#' @describeIn packages Return the parsed package version.
#' @export
pkg_version <- function(path = ".") {
  desc_desc_get_version(pkg_path(path))
}

#' @describeIn packages Return the raw package version (as a string).
#' @export
pkg_version_raw <- function(path = ".") {
  desc_desc_get_field("Version", file = pkg_path(path))[[1]]
}

#' @describeIn packages Return the package namespace.
#' @export
pkg_ns <- function(path = ".") {
  ns_env(pkg_name(path))
}
