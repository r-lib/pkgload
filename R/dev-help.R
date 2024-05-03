#' In-development help for package loaded with devtools
#'
#' `dev_help()` searches for source documentation provided in packages
#' loaded by devtools. To improve performance, the `.Rd` files are
#' parsed to create to index once, then cached. Use
#' `dev_topic_index_reset()` to clear that index. You can manually
#' retrieve the index for a local package with `dev_topic_index()`.
#'
#' @param topic name of help to search for.
#' @param dev_packages A character vector of package names to search within.
#'   If `NULL`, defaults to all packages loaded by devtools.
#' @param stage at which stage ("build", "install", or "render") should
#'   `\\Sexpr` macros be executed? This is only important if you're using
#'   `\\Sexpr` macro's in your Rd files.
#' @param type of html to produce: `"html"` or `"text"`. Defaults to
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

  if (length(dev_packages()) == 0) {
    cli::cli_abort(c(
      "Can't find development documentation because no in-development packages loaded.",
      i = "Do you need to run {.run pkgload::load_all()}?"
    ))
  }

  loc <- dev_topic_find(topic, dev_packages)

  if (!is.null(loc$path) && !fs::file_exists(loc$path)) {
    # Documentation topic might have moved, so reset topic index and try again
    dev_topic_index_reset(loc$pkg)
    loc <- dev_topic_find(topic, dev_packages)
  }

  if (is.null(loc$path) || !fs::file_exists(loc$path)) {
    cli::cli_abort("Can't find development topic {.arg {topic}}.")
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

has_rd_macros <- function(dir) {
  desc <- file.path(dir, "DESCRIPTION")
  if (!file.exists(desc)) {
    return(FALSE)
  }

  tryCatch(
    expr = {
      desc <- read.dcf(desc)
      "RdMacros" %in% colnames(desc)
    },
    error = function(...) FALSE
  )
}

load_rd_macros <- function(dir) {
  macros <- tools::loadPkgRdMacros(dir)
  tools::loadRdMacros(
    file.path(R.home("share"), "Rd", "macros", "system.Rd"),
    macros = macros
  )
}

#' @export
print.dev_topic <- function(x, ...) {
  cli::cli_inform(c("i" = "Rendering development documentation for {.val {x$topic}}"))

  type <- arg_match0(x$type %||% "text", c("text", "html"))

  # Use rstudio's previewRd() if possible
  if (type == "html" && rstudioapi_available()) {
    # If the package has Rd macros, this needs a version of rstudio
    # that loads them, see rstudio/rstudio#12111
    version_needed <- if (has_rd_macros(dirname(dirname(x$path)))) "2022.12.0.256"

    if (rstudioapi::hasFun("previewRd", version_needed = version_needed)) {
      return(rstudioapi::callFun("previewRd", x$path))
    }
  }

  # otherwise render and serve
  file <- fs::path_ext_set(fs::path_file(x$path), type)

  # This directory structure is necessary for RStudio to open the
  # .html file in the help pane (see rstudio/rstudio#11336)
  doc_path <- fs::path("doc", "html", file)
  path <- fs::path(tempdir(), ".R", doc_path)
  fs::dir_create(fs::path_dir(path), recurse = TRUE)

  if (type == "text") {
    topic_write_text(x, path)
    title <- paste(x$pkg, basename(x$path), sep = ":")
    file.show(path, title = title)
  } else if (type == "html") {
    topic_write_html(x, path)

    if (is_rstudio()) {
      # This localhost URL is also part of getting RStudio to open in
      # the help pane
      port <- httpdPort()
      url <- sprintf("http://localhost:%i/%s", port, doc_path)
    } else {
      url <- path
    }

    utils::browseURL(url)
  }
}

on_load(
  httpdPort %<~% env_get(rlang::ns_env("tools"), "httpdPort")
)

topic_write_text <- function(x, path) {
  macros <- load_rd_macros(dirname(dirname(x$path)))

  tools::Rd2txt(
    x$path,
    out = path,
    package = x$pkg,
    stages = x$stage,
    macros = macros
  )
}

topic_write_html <- function(x, path) {
  macros <- load_rd_macros(dirname(dirname(x$path)))

  tools::Rd2HTML(
    x$path,
    out = path,
    package = x$pkg,
    stages = x$stage,
    no_links = TRUE,
    macros = macros
  )

  css_path <- file.path(tempdir(), "R.css")
  if (!file.exists(css_path)) {
    file.copy(file.path(R.home("doc"), "html", "R.css"), css_path)
  }
}

topic_lines <- function(x, type = c("text", "html")) {
  file <- withr::local_tempfile()

  switch(
    arg_match(type),
    text = topic_write_text(x, file),
    html = topic_write_html(x, file)
  )

  readLines(file)
}

#' Drop-in replacements for help and ? functions
#'
#' The `?` and `help` functions are replacements for functions of the
#' same name in the utils package. They are made available when a package is
#' loaded with [load_all()].
#'
#' The `?` function is a replacement for [utils::?()] from the
#' utils package. It will search for help in devtools-loaded packages first,
#' then in regular packages.
#'
#' The `help` function is a replacement for [utils::help()] from
#' the utils package. If `package` is not specified, it will search for
#' help in devtools-loaded packages first, then in regular packages. If
#' `package` is specified, then it will search for help in devtools-loaded
#' packages or regular packages, as appropriate.
#'
#' @param topic A name or character string specifying the help topic.
#' @param package A name or character string specifying the package in which
#'   to search for the help topic. If NULL, search all packages.
#' @param e1 First argument to pass along to `utils::`?``.
#' @param e2 Second argument to pass along to `utils::`?``.
#' @param ... Additional arguments to pass to [utils::help()].
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

  is_string <- tryCatch(
    error = function(...) FALSE,
    {
      force(topic)
      is_string(topic)
    }
  )

  if (is_string) {
    topic_str <- topic
    topic_name <- sym(topic)
  } else if (missing(topic_name)) {
    # Leave the vars missing
  } else if (is_null(topic_name)) {
    topic_str <- NULL
    topic_name <- NULL
  } else {
    topic_str <- deparse(substitute(topic))
    if (length(topic_str) != 1) {
      cli::cli_abort("{.arg topic} must be a name.")
    }
  }

  # help's NSE for package is slightly simpler
  package_name <- substitute(package)
  if (is_symbol(package_name)) {
    package_str <- as_string(package_name)
  } else {
    # Complex expression, just evaluate it (#266). The value is
    # injected in `utils::help(package = )` below, causing it to be
    # interpreted as is.
    package_str <- package
    package_name <- package
  }

  use_dev <-
    (!missing(topic) && is_string(package_str) && package_str %in% dev_packages()) ||
    (!missing(topic_name) && is_null(package_str) && !is_null(dev_topic_find(topic_str)))

  if (use_dev) {
    dev_help(topic_str, package_str)
  } else {
    inject(utils::help(
      !!maybe_missing(topic_name),
      !!maybe_missing(package_name),
      ...
    ))
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
    cli::cli_abort("Unknown input.")
  }

  # Search for the topic in devtools-loaded packages.
  # If not found, call utils::`?`.
  if (!is.null(topic) && !is.null(dev_topic_find(topic, pkg))) {
    dev_help(topic, pkg)
  } else {
    eval(as.call(list(utils::`?`, substitute(e1), substitute(e2))))
  }
}
