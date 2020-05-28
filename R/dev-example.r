#' Run a examples for an in-development function.
#'
#' `dev_example` is a replacement for `example`. `run_example`
#' is a low-level function that takes a path to an Rd file.
#'
#' @param topic Name or topic (or name of Rd) file to run examples for
#' @param quiet If `TRUE`, does not echo code to console.
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

  run_example(topic$path, quiet = quiet)
}

#' @rdname dev_example
#' @export
#' @param path Path to `.Rd` file
#' @param test if `TRUE`, code in \code{\\donttest{}} will be commented
#'   out. If `FALSE`, code in \code{\\testonly{}} will be commented out. This
#'   parameter is only used in R 3.2 and greater.
#' @param run if `TRUE`, code in \code{\\dontrun{}} will be commented
#'   out.
#' @param env Environment in which code will be run.
#' @param macros Custom macros to use to parse the `.Rd` file. See the
#'   `macros` argument of [tools::parse_Rd()]. If `NULL`, then the
#'   [tools::Rd2ex()] (and [tools::parse_Rd()]) default is used.
run_example <- function(path, test = FALSE, run = FALSE,
                        env = new.env(parent = globalenv()),
                        quiet = FALSE, macros = NULL) {

  if (!file.exists(path)) {
    stop("'", path, "' does not exist", call. = FALSE)
  }

  tmp <- tempfile(fileext = ".R")

  if (is.null(macros)) {
    tools::Rd2ex(
      path,
      out = tmp,
      commentDontrun = !run,
      commentDonttest = !test
    )
  } else {
    tools::Rd2ex(
      path,
      out = tmp,
      commentDontrun = !run,
      commentDonttest = !test,
      macros = macros
    )
  }

  if (file.exists(tmp)) {
    source(tmp, echo = !quiet, local = env, max.deparse.length = Inf)
  }

  invisible(env)
}
