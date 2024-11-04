#' @useDynLib testDllcpp11, .registration = TRUE
"_PACKAGE"

#' Hello World
#' @export
cpp11_hello_world <- function() {
  cpp11_hello_world_()
}

#' Test Attributes
#' @export
cpp11_test_attributes <- function() {
  cpp11_test_attributes_()
}
