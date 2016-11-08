#' Run a examples for an in-development function.
#'
#' @inheritParams run_examples
#' @param topic Name or topic (or name of Rd) file to run examples for
#' @param quiet If \code{TRUE}, runs example quietly.
#' @export
#' @family example functions
#' @examples
#' \dontrun{
#' # Runs installed example:
#' library("ggplot2")
#' example("ggplot")
#'
#' # Runs develoment example:
#' dev_example("ggplot")
#' }
dev_example <- function(topic, quiet = FALSE) {
  topic <- dev_help(topic)

  load_all(topic$pkg, quiet = quiet)
  run_example(topic$path, quiet = quiet)
}

run_example <- function(path, show = TRUE, test = FALSE, run = FALSE,
                        env = new.env(parent = globalenv()),
                        quiet = FALSE) {

  tmp <- tempfile(fileext = ".R")
  tools::Rd2ex(path, out = tmp, commentDontrun = !run, commentDonttest = !test)
  source(tmp, echo = !quiet, local = env, max.deparse.length = Inf)

  invisible(env)
}
