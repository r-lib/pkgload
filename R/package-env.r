# Create the package environment where exported objects will be copied to
attach_ns <- function(package) {
  nsenv <- ns_env(package)

  if (is_attached(package)) {
    stop("Package ", package, " is already attached.")
  }

  # This should be similar to attachNamespace
  pkgenv <- base::attach(NULL, name = pkg_env_name(package))
  attr(pkgenv, "path") <- getNamespaceInfo(nsenv, "path")

  invisible(pkgenv)
}

# Invoke namespace load actions. According to the documentation for setLoadActions
# these actions should be called at the end of processing of S4 metadata, after
# dynamically linking any libraries, the call to .onLoad(), if any, and caching
# method and class definitions, but before the namespace is sealed
run_ns_load_actions <- function(package) {
  nsenv <- ns_env(package)
  actions <- methods::getLoadActions(nsenv)
  for (action in actions)
    action(nsenv)
}

# Copy over the objects from the namespace env to the package env
export_ns <- function(package) {
  nsenv <- ns_env(package)
  pkgenv <- pkg_env(package)
  ns_path <- ns_path(nsenv)
  nsInfo <- parse_ns_file(ns_path)

  exports <- getNamespaceExports(nsenv)
  importIntoEnv(pkgenv, exports, nsenv, exports)

  desc <- pkg_desc(ns_path(package))

  # If lazydata is true, manually copy data objects in $lazydata to package
  # environment
  lazydata <- desc$get("LazyData")
  if (!is.na(lazydata) &&
      tolower(lazydata) %in% c("true", "yes")) {
    copy_env(src = nsenv$.__NAMESPACE__.$lazydata, dest = pkgenv)
  }
}


#' Return package environment
#'
#' This is an environment like `<package:pkg>`. The package
#' environment contains the exported objects from a package. It is
#' attached, so it is an ancestor of `R_GlobalEnv`.
#'
#' When a package is loaded the normal way, using [library()],
#' this environment contains only the exported objects from the
#' namespace. However, when loaded with [load_all()], this
#' environment will contain all the objects from the namespace, unless
#' `load_all` is used with `export_all=FALSE`.
#'
#' If the package is not attached, this function returns `NULL`.
#'
#' @inheritParams ns_env
#' @keywords internal
#' @seealso [ns_env()] for the namespace environment that
#'   all the objects (exported and not exported).
#' @seealso [imports_env()] for the environment that contains
#'   imported objects for the package.
#' @export
pkg_env <- function(package) {
  name <- pkg_env_name(package)

  if (!is_attached(package)) return(NULL)

  as.environment(name)
}


# Generate name of package environment
# Contains exported objects
pkg_env_name <- function(package) {
  paste("package:", package, sep = "")
}


# Reports whether a package is loaded and attached
is_attached <- function(package) {
  pkg_env_name(package) %in% search()
}
