#' @export
as.character.pkgload_foobar <- function(x, ...) {
  "registered"
}

#' @export
my_generic <- function(x) {
  UseMethod("my_generic")
}
#' @export
my_generic.pkgload_foobar <- function(x) {
  "registered"
}
