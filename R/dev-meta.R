#' Return devtools metadata environment
#'
#' If the package was not loaded with devtools, returns `NULL`.
#'
#' @param name The name of a loaded package
#' @keywords internal
#' @examples
#' dev_meta("stats") # NULL
#'
#' if (has_tests()) {
#' # Load the test package in directory "testLoadHooks"
#' load_all(pkgtest("testLoadHooks"))
#'
#' # Get metadata for the package
#' x <- dev_meta("testLoadHooks")
#' as.list(x)
#'
#' # Clean up.
#' unload("testLoadHooks")
#' }
#' @export
dev_meta <- function(name) {
  ns <- .getNamespace(name)
  if (is.null(ns)) {
    cli::cli_abort(c(
      "Namespace not found for {.arg {name}}",
      "i" = "Is it loaded?"
    ))
  }

  if (is.null(ns$.__DEVTOOLS__)) {
    return(NULL)
  }

  ns$.__DEVTOOLS__
}


# Create the devtools metadata environment for a package.
# This should be run when packages are loaded by devtools.
create_dev_meta <- function(name) {
  ns <- .getNamespace(name)

  if (!is.null(ns$.__DEVTOOLS__)) {
    cli::cli_abort(
      "Devtools metadata for package {.pkg {name}} can't already exist."
    )
  }

  ns$.__DEVTOOLS__ <- new.env(parent = ns)
  ns$.__DEVTOOLS__
}
