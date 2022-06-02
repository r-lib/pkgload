#' Return imports environment for a package
#'
#' Contains objects imported from other packages. Is the parent of the
#' package namespace environment, and is a child of `<namespace:base>`,
#' which is a child of `R_GlobalEnv`.
#' @keywords internal
#' @param path TODO: fix doc
#' @seealso [ns_env()] for the namespace environment that
#'   all the objects (exported and not exported).
#' @seealso [pkg_env()] for the attached environment that contains
#'   the exported objects.
#' @export
imports_env <- function(package) {
  if (!is_loaded(package)) {
    cli::cli_abort("Namespace environment must be created before accessing imports environment.")
  }

  env <- parent.env(ns_env(package))

  if (attr(env, 'name') != imports_env_name(package)) {
    cli::cli_abort(c(
      "Imports environment does not have attribute {.arg name} with value {imports_env_name(package)}.",
      "i" = "This probably means that the namespace environment was not created correctly."
    ))
  }

  env
}


# Generate name of package imports environment
# Contains exported objects
imports_env_name <- function(package) {
  paste("imports:", package, sep = "")
}


#' Load all of the imports for a package
#'
#' The imported objects are copied to the imports environment, and are not
#' visible from `R_GlobalEnv`. This will automatically load (but not attach)
#' the dependency packages.
#'
#' @keywords internal
load_imports <- function(path = ".") {
  package <- pkg_name(path)
  description <- pkg_desc(path)

  # Get data frame of dependency names and versions
  deps <- description$get_deps()
  imports <- deps[deps$type == "Imports", ]

  if (length(imports) == 0) {
    return(invisible())
  }

  # If we've already loaded imports, don't load again (until load_all
  # is run with reset=TRUE). This is to avoid warnings when running
  # process_imports()
  if (length(ls(envir = imports_env(package))) > 0) {
    return(invisible(imports))
  }

  deps_check_installed(path, imports)
  process_imports(path)

  invisible(deps)
}

# Load imported objects
# The code in this function is taken and adapted from base::loadNamespace
# Setup variables were added and the for loops put in a tryCatch block
# https://github.com/wch/r-source/blob/tags/R-3-3-0/src/library/base/R/namespace.R#L397-L427

# This wraps the inner for loop iterations in a tryCatch
wrap_inner_loop <- function(x) {
  inner <- x[[4]]
  x[[4]] <- call("tryCatch", error = quote(warning), inner)
  x
}

load_namespace_for1 <- function() wrap_inner_loop(
  extract_lang(body(loadNamespace), comp_lang, y = quote(for(i in nsInfo$imports) NULL), idx = 1:3)
  )
load_namespace_for2 <- function() wrap_inner_loop(
  extract_lang(body(loadNamespace), comp_lang,
    y = quote(for(imp in nsInfo$importClasses) NULL),
    idx = 1:3)
  )
load_namespace_for3 <- function() wrap_inner_loop(
  extract_lang(body(loadNamespace), comp_lang,
    y = quote(for(imp in nsInfo$importMethods) NULL),
    idx = 1:3)
  )

onload_assign("process_imports", {

  process_imports <- function(path = ".") {
    path <- pkg_path(path)
    package <- pkg_name(path)
    desc_path <- package_file("DESCRIPTION", path = path)
    vI <- ("tools" %:::% ".split_description")(("tools" %:::% ".read_description")(desc_path))$Imports
    nsInfo <- parse_ns_file(path)
    ns <- ns_env(package)
    lib.loc <- NULL

    !! load_namespace_for1()
    !! load_namespace_for2()
    !! load_namespace_for3()
  }

  process_imports <- expr_interp(process_imports)
  fn_env(process_imports) <- rlang::ns_env("pkgload")

  process_imports
})
