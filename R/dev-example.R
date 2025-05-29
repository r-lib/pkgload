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
#' # Runs development example:
#' dev_example("ggplot")
#' }
dev_example <- function(topic, quiet = FALSE) {
  topic <- dev_help(topic)

  run_example(topic$path, quiet = quiet)
}

#' @rdname dev_example
#' @export
#' @param path Path to `.Rd` file
#' @param run_dontrun if `TRUE`, do run `\dontrun` sections in the Rd files.
#' @param run_donttest if `TRUE`, do run `\donttest` sections in the Rd files.
#' @param env Environment in which code will be run.
#' @param macros Custom macros to use to parse the `.Rd` file. See the
#'   `macros` argument of [tools::parse_Rd()]. If `NULL`, then the
#'   [tools::Rd2ex()] (and [tools::parse_Rd()]) default is used.
#' @param run,test Deprecated, see `run_dontrun` and `run_donttest` above.
run_example <- function(
  path,
  run_donttest = FALSE,
  run_dontrun = FALSE,
  env = new.env(parent = globalenv()),
  quiet = FALSE,
  macros = NULL,
  run,
  test
) {
  if (!missing(run)) {
    cli::cli_warn(
      "{.code run_example(run =)} is deprecated, please use {.code run_example(run_dontrun =)} instead"
    )
    run_dontrun <- run
  }
  if (!missing(test)) {
    cli::cli_warn(
      "{.code run_example(test =)} is deprecated, please use {.code run_example(run_donttest =)} instead"
    )
    run_donttest <- test
  }
  if (!file.exists(path)) {
    cli::cli_abort("The path {.path {path}} must exit.")
  }

  tmp <- tempfile(fileext = ".R")

  if (is.null(macros)) {
    tools::Rd2ex(
      path,
      out = tmp,
      commentDontrun = !run_dontrun,
      commentDonttest = !run_donttest
    )
  } else {
    tools::Rd2ex(
      path,
      out = tmp,
      commentDontrun = !run_dontrun,
      commentDonttest = !run_donttest,
      macros = macros
    )
  }

  if (file.exists(tmp)) {
    source(tmp, echo = !quiet, local = env, max.deparse.length = Inf)
  }

  invisible(env)
}
