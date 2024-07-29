# Given the name or vector of names, returns a named vector reporting
# whether each exists and is a directory.
dir.exists <- function(x) {
  res <- file.exists(x) & file.info(x)$isdir
  stats::setNames(res, x)
}

compact <- function(x) {
  is_empty <- vapply(x, function(x) length(x) == 0, logical(1))
  x[!is_empty]
}

"%||%" <- function(a, b) if (!is.null(a)) a else b

"%:::%" <- function(p, f) {
  get(f, envir = asNamespace(p))
}

is_installed <- function(package, version = "0") {
  installed_version <- tryCatch(utils::packageVersion(package), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

#' Check that the version of an suggested package satisfies the requirements
#'
#' @param package The name of the suggested package
#' @param version The version of the package
#' @param compare The comparison operator used to check the version
#' @keywords internal
#' @export
check_suggested <- function(package, version = NULL, compare = NA, path = inst("pkgload")) {
  if (is.null(version)) {
    if (!is.na(compare)) {
      cli::cli_abort("Must provide both {.arg compare} and {.arg version}.")
    }
    version <- suggests_dep(package, path = path)
  }

  if (!is_installed(package) || !check_dep_version(package, version)) {
    cli_version <- if (is_na(version)) "" else paste0(" ", version)
    msg <- c("{.pkg package}{cli_version} must be installed for this functionality.")

    if (interactive()) {
      cli::cli_inform(c("i" = msg))
      cli::cli_inform(c("!" = "Would you like to install it?"))
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(package)
        return()
      }
    }
    cli::cli_abort(msg)
  }
}

suggests_dep <- function(package, path = inst("pkgload")) {
  desc <- pkg_desc(path)$get_deps()
  found <- desc[desc$type == "Suggests" & desc$package == package, "version"]

  if (!length(found)) {
    cli::cli_abort("{.pkg {package}} is not in {.code Suggests:} for {.pkg {pkg_name(path)}}.")
  }

  found
}

all_named <- function (x) {
  if (length(x) == 0) return(TRUE)
  !is.null(names(x)) && all(names(x) != "")
}

make_function <- function (args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  stopifnot(all_named(args), is.language(body))
  eval(call("function", args, body), env)
}

comp_lang <- function(x, y, idx = seq_along(y)) {
  if (is.symbol(x) || is.symbol(y)) {
    return(identical(x, y))
  }

  if (length(x) < length(idx)) return(FALSE)

  identical(x[idx], y[idx])
}

extract_lang <- function(x, f, ...) {
  recurse <- function(y) {
    unlist(compact(lapply(y, extract_lang, f = f, ...)), recursive = FALSE)
  }

  # if x matches predicate return it
  if (isTRUE(f(x, ...))) {
    return(x)
  }

  if (is.call(x)) {
    res <- recurse(x)[[1]]
    if (top_level_call <- identical(sys.call()[[1]], as.symbol("extract_lang"))
        && is.null(res)) {
      cli::cli_warn(c(
        "pkgload is incompatible with the current version of R.",
        "i" = "{.code load_all()} may function incorrectly."
      ))
    }
    return(res)
  }

  NULL
}

modify_lang <- function(x, f, ...) {
  recurse <- function(x) {
    lapply(x, modify_lang, f = f, ...)
  }

  x <- f(x, ...)

  if (is.call(x)) {
    as.call(recurse(x))
  } else if (is.function(x)) {
     formals(x) <- modify_lang(formals(x), f, ...)
     body(x) <- modify_lang(body(x), f, ...)
     x
  } else {
    x
  }
}

strip_internal_calls <- function(x, package) {
  if (is.call(x) && identical(x[[1L]], as.name(":::")) && identical(x[[2L]], as.name(package))) {
    x[[3L]]
  } else {
    x
  }
}

sort_ci <- function(x) {
  local_collate("C")
  x[order(tolower(x), x)]
}

dev_packages <- function() {
  packages <- vapply(loadedNamespaces(),
    function(x) !is.null(dev_meta(x)), logical(1))

  names(packages)[packages]
}

copy_env <- function(src, dest = new.env(parent = emptyenv()),
  ignore = NULL) {

  srclist <- as.list(src, all.names = TRUE)
  srclist <- srclist[ !(names(srclist) %in% ignore) ]
  list2env(srclist, envir = dest)

  dest
}

copy_env_lazy <- function(src, dest = new.env(parent = emptyenv()),
  ignore = NULL) {

  nms <- ls(src, all.names = TRUE)
  nms <- nms[ !(nms %in% ignore) ]
  for (nme in nms) {
    delayed_assign(nme, as.symbol(nme), eval.env = src, assign.env = dest)
  }

  dest
}

# A version of delayedAssign which does _not_ use substitute
delayed_assign <- function(x, value, eval.env = parent.frame(1), assign.env = parent.frame(1)) {
  inject(delayedAssign(x, !!value, eval.env, assign.env))
}

last <- function(x) utils::tail(x, n = 1L)

single_quote <- function(x) {
  encodeString(x, quote = "'")
}

ns_s3_methods <- function(pkg) {
 ns_env(pkg)$.__S3MethodsTable__.
}

paste_line <- function(...) {
  paste(c(...), collapse = "\n")
}

style_hyperlink_run <- function(code) {
  if (nzchar(Sys.getenv("R_CLI_HAS_HYPERLINK_RUN"))) {
    link <- paste0("rstudio:run:", code)
    code <- cli::style_hyperlink(code, link)
  }

  cli::format_inline("{.code {code}}")
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

is_rstudio <- function() {
  is_string(.Platform$GUI, "RStudio")
}

rstudioapi_available <- function() {
  is_installed("rstudioapi") && rstudioapi::isAvailable()
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

catch_and_warn <- function(expr) {
  tryCatch(
    expr,
    error = function(cnd) warn(function(w, ...) conditionMessage(cnd))
  )
}
