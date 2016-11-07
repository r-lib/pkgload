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

is_installed <- function(pkg, version = 0) {
  installed_version <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  !is.na(installed_version) && installed_version >= version
}

check_suggested <- function(pkg, version = NULL, compare = NA) {

  if (is.null(version)) {
    if (!is.na(compare)) {
      stop("Cannot set ", sQuote(compare), " without setting ",
           sQuote(version), call. = FALSE)
    }

    dep <- suggests_dep(pkg)

    version <- dep$version
    compare <- dep$compare
  }

  if (!is_installed(pkg) || !check_dep_version(pkg, version, compare)) {
    msg <- paste0(sQuote(pkg),
      if (is.na(version)) "" else paste0(" >= ", version),
      " must be installed for this functionality.")

    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}

suggests_dep <- function(pkg) {

  suggests <- read_dcf(system.file("DESCRIPTION", package = "pkgload"))$Suggests
  deps <- parse_deps(suggests)

  found <- which(deps$name == pkg)[1L]

  if (!length(found)) {
     stop(sQuote(pkg), " is not in Suggests: for pkgload!", call. = FALSE)
  }
  deps[found, ]
}

read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  as.list(read.dcf(path, keep.white = fields)[1, ])
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
      warning("Devtools is incompatible with the current version of R. `load_all()` may function incorrectly.")
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
  withr::with_collate("C", x[order(tolower(x), x)])
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
