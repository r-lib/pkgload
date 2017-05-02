#' Test function for help
#'
#' The purpose of this function is to test out \code{help} and \code{?} from
#' devtools.
#'
#' @export
#' @examples
#' a <- 101
foofoo <- function() "You called foofoo."

#' testHelp: some title
#' Some description
"_PACKAGE"

#' Function level help for testHelp
#'
#' @export
testHelp <- function() "You called testHelp"
