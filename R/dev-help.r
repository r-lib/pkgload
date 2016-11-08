#' In-development help for package loaded with devtools
#'
#' \code{dev_help} searches for source documentation provided in
#' packages loaded by devtools. To improve performance, the \code{.Rd} files
#' are parsed to create to index once, then cached. Use
#' \code{dev_topic_index_reset} to clear that index.
#'
#' @param topic name of help to search for.
#' @param dev_packages A character vector of package names to search within.
#'   If \code{NULL}, defaults to all packages loaded by devtools.
#' @param stage at which stage ("build", "install", or "render") should
#'   \\Sexpr macros be executed? This is only important if you're using
#'   \\Sexpr macro's in your Rd files.
#' @param type of html to produce: \code{"html"} or \code{"text"}. Defaults to
#'   your default documentation type.
#' @export
#' @examples
#' \dontrun{
#' library("ggplot2")
#' help("ggplot") # loads installed documentation for ggplot
#'
#' load_all("ggplot2")
#' dev_help("ggplot") # loads development documentation for ggplot
#' }
dev_help <- function(topic,
                     dev_packages = NULL,
                     stage = "render",
                     type = getOption("help_type")) {
  loc <- dev_topic_find(topic, dev_packages)

  if (is.null(loc$path)) {
    stop("Could not find development topic '", topic, "'", call. = FALSE)
  }

  structure(
    list(
      topic = topic,
      pkg = loc$pkg,
      path = loc$path,
      stage = stage,
      type = type
    ),
    class = "dev_topic"
  )
}

#' @export
print.dev_topic <- function(x, ...) {
  message("Rendering development documentation for '", x$topic, "'")

  if (rstudioapi::hasFun("previewRd")) {
    rstudioapi::callFun("previewRd", x$path)
    return(invisible())
  }

  type <- match.arg(x$type %||% "text", c("text", "html"))
  out_path <- paste(tempfile("Rtxt"), type, sep = ".")

  if (type == "text") {
    tools::Rd2txt(x$path, out = out_path, package = x$package, stages = x$stage)
    file.show(out_path, title = paste(x$package, basename(x$path), sep = ":"))
  } else if (type == "html") {
    tools::Rd2HTML(x$path, out = out_path, package = x$package, stages = x$stage,
      no_links = TRUE)

    css_path <- file.path(tempdir(), "R.css")
    if (!file.exists(css_path)) {
      file.copy(file.path(R.home("doc"), "html", "R.css"), css_path)
    }

    utils::browseURL(out_path)
  }
}


#' Drop-in replacements for help and ? functions
#'
#' The \code{?} and \code{help} functions are replacements for functions of the
#' same name in the utils package. They are made available when a package is
#' loaded with \code{\link{load_all}}.
#'
#' The \code{?} function is a replacement for \code{\link[utils]{?}} from the
#' utils package. It will search for help in devtools-loaded packages first,
#' then in regular packages.
#'
#' The \code{help} function is a replacement for \code{\link[utils]{help}} from
#' the utils package. If \code{package} is not specified, it will search for
#' help in devtools-loaded packages first, then in regular packages. If
#' \code{package} is specified, then it will search for help in devtools-loaded
#' packages or regular packages, as appropriate.
#'
#' @inheritParams utils::help utils::`?`
#' @param topic A name or character string specifying the help topic.
#' @param package A name or character string specifying the package in which
#'   to search for the help topic. If NULL, seach all packages.
#' @param e1 First argument to pass along to \code{utils::`?`}.
#' @param e2 Second argument to pass along to \code{utils::`?`}.
#' @param ... Additional arguments to pass to \code{\link[utils]{help}}.
#'
#' @rdname help
#' @name help
#' @usage # help(topic, package = NULL, ...)
#'
#' @examples
#' \dontrun{
#' # This would load devtools and look at the help for load_all, if currently
#' # in the devtools source directory.
#' load_all()
#' ?load_all
#' help("load_all")
#' }
#'
#' # To see the help pages for utils::help and utils::`?`:
#' help("help", "utils")
#' help("?", "utils")
#'
#' \dontrun{
#' # Examples demonstrating the multiple ways of supplying arguments
#' # NB: you can't do pkg <- "ggplot2"; help("ggplot2", pkg)
#' help(lm)
#' help(lm, stats)
#' help(lm, 'stats')
#' help('lm')
#' help('lm', stats)
#' help('lm', 'stats')
#' help(package = stats)
#' help(package = 'stats')
#' topic <- "lm"
#' help(topic)
#' help(topic, stats)
#' help(topic, 'stats')
#' }
shim_help <- function(topic, package = NULL, ...) {
  # Reproduce help's NSE for topic - try to eval it and see if it's a string
  topic_name <- substitute(topic)
  is_char <- FALSE
  try(is_char <- is.character(topic) && length(topic) == 1L, silent = TRUE)
  if (is_char) {
    topic_str <- topic
    topic_name <- as.name(topic)
  } else if (missing(topic_name)) {
    # Leave the vars missing
  } else if (is.null(topic_name)) {
    topic_str <- NULL
    topic_name <- NULL
  } else {
    topic_str <- deparse(substitute(topic))
  }

  # help's NSE for package is slightly simpler
  package_name <- substitute(package)
  if (is.name(package_name)) {
    package_str <- as.character(package_name)
  } else if (is.null(package_name)) {
    package_str <- NULL
  } else {
    package_str <- package
    package_name <- as.name(package)
  }

  use_dev <- (!is.null(package_str) && package_str %in% dev_packages()) ||
    (is.null(package_str) && !is.null(dev_topic_find(topic_str)))
  if (use_dev) {
    dev_help(topic_str, package_str)
  } else {
    # This is similar to list(), except that one of the args is a missing var,
    # it will replace it with an empty symbol instead of trying to evaluate it.
    as_list <- function(..., .env = parent.frame()) {
      dots <- match.call(expand.dots = FALSE)$`...`

      lapply(dots, function(var) {
        is_missing <- eval(substitute(missing(x), list(x = var)), .env)
        if (is_missing) {
          quote(expr=)
        } else {
          eval(var, .env)
        }
      })
    }

    call <- substitute(
      utils::help(topic, package, ...),
      as_list(topic = topic_name, package = package_name)
    )
    eval(call)
  }
}


#' @usage
#' # ?e2
#' # e1?e2
#'
#' @rdname help
#' @name ?
shim_question <- function(e1, e2) {
  # Get string version of e1, for find_topic
  e1_expr <- substitute(e1)
  if (is.name(e1_expr)) {
    # Called with a bare symbol, like ?foo
    topic <- as.character(e1_expr)
    pkg <- NULL
  } else if (is.call(e1_expr)) {
    if (identical(e1_expr[[1]], quote(`?`))) {
      # ??foo
      topic <- NULL
      pkg <- NULL
    } else if (identical(e1_expr[[1]], quote(`::`))) {
      # ?bar::foo
      topic <- as.character(e1_expr[[3]])
      pkg <- as.character(e1_expr[[2]])
    } else {
      # ?foo(12)
      topic <- deparse(e1_expr[[1]])
      pkg <- NULL
    }
  } else if (is.character(e1_expr)) {
    topic <- e1
    pkg <- NULL
  } else {
    stop("Unknown input", call. = FALSE)
  }

  # Search for the topic in devtools-loaded packages.
  # If not found, call utils::`?`.
  if (!is.null(topic) && !is.null(dev_topic_find(topic, pkg))) {
    dev_help(topic, pkg)
  } else {
    eval(as.call(list(utils::`?`, substitute(e1), substitute(e2))))
  }
}
