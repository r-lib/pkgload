setup_pkg_env <- function(pkg) {
  if (!is_attached(pkg)) {
    attach_ns(pkg)
  }

  # Copy over lazy data objects from the namespace environment
  export_lazydata(pkg)

  # Copy over objects from the namespace environment
  export_ns(pkg)

  # Assign .Depends, if any, to package environment from namespace
  assign_depends(pkg)
}

# Create the package environment where exported objects will be copied to
attach_ns <- function(package) {
  nsenv <- ns_env(package)

  if (is_attached(package)) {
    cli::cli_abort("Package {.pkg {package}} can't be already attached.")
  }

  # This should be similar to attachNamespace
  pkgenv <- base::attach(NULL, name = pkg_env_name(package))
  attr(pkgenv, "path") <- getNamespaceInfo(nsenv, "path")

  invisible(pkgenv)
}

populate_pkg_env <- function(pkg,
                             path,
                             export_all,
                             export_imports,
                             helpers) {
  pkg_env <- pkg_env(pkg)

  if (export_all) {
    env_coalesce(pkg_env, ns_env(pkg))

    if (export_imports) {
      env_coalesce(pkg_env, imports_env(pkg))
    }

    env_unbind(pkg_env, exports_exclusion_list)
  }

  # Source test helpers into pkg environment
  if (helpers && uses_testthat(path)) {
    local_envvar(NOT_CRAN = "true")
    testthat_source_test_helpers(find_test_dir(path), env = pkg_env)
  }
}

# Namespace and devtools bindings to exclude from package envs
exports_exclusion_list <- c(
  ".__NAMESPACE__.",
  ".__S3MethodsTable__.",
  ".packageName",
  ".First.lib",
  ".onLoad",
  ".onAttach",
  ".conflicts.OK",
  ".noGenerics",
  ".__DEVTOOLS__",
  ".cache"
)

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

}

export_lazydata <- function(package) {
  nsenv <- ns_env(package)
  pkgenv <- pkg_env(package)
  desc <- pkg_desc(ns_path(package))

  # If lazydata is true, manually copy data objects in $lazydata to package
  # environment
  lazydata <- desc$get("LazyData")
  if (!is.na(lazydata) &&
      tolower(lazydata) %in% c("true", "yes")) {
    copy_env_lazy(src = nsenv$.__NAMESPACE__.$lazydata, dest = pkgenv)
  }
}

# Assign `.Depends` from namespace
assign_depends <- function(package) {
  pkgenv <- pkg_env(package)

  desc <- pkg_desc(ns_path(package))
  deps <- desc$get_deps()
  depends <- unique(deps[deps$type == "Depends"
                         & deps$package != "R",]$package)
  if (length(depends) > 0L) pkgenv$.Depends <- depends
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
