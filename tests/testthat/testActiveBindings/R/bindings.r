
makeActiveBinding("foo", function() rlang::abort("foo"), environment())

#' @export
makeActiveBinding("bar", function() rlang::abort("bar"), environment())
