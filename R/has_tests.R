#' Was devtools installed with tests?
#'
#' @keywords internal
#' @export
has_tests <- function() {
  system.file("tests", package = "pkgload") != ""
}

#' Return the path to one of the packages in the devtools test dir
#'
#' devtools comes with some simple packages for testing. This function
#' returns the path to them.
#'
#' @param package Name of the test package.
#' @keywords internal
#' @examples
#' if (has_tests()) {
#' pkgtest("testData")
#' }
#' @export
pkgtest <- function(package) {
  stopifnot(has_tests())

  path <- system.file(package = "pkgload", "tests", "testthat", package)
  if (path == "") stop(package, " not found", call. = FALSE)

  path
}
