#' Return the namespace environment for a package.
#'
#' Contains all (exported and non-exported) objects, and is a descendant of
#' `R_GlobalEnv`. The hierarchy is `<namespace:pkg>`,
#' `<imports:pkg>`, `<namespace:base>`, and then
#' `R_GlobalEnv`.
#'
#' If the package is not loaded, this function returns `NULL`.
#'
#' @param package package name.
#' @keywords internal
#' @seealso [pkg_env()] for the attached environment that
#'   contains the exported objects.
#' @seealso [imports_env()] for the environment that contains
#'   imported objects for the package.
#' @export
ns_env <- function(package) {
  if (!is_loaded(package)) return(NULL)

  asNamespace(package)
}

ns_path <- function(package) {
  ns <- asNamespace(package)
  if (isBaseNamespace(ns))
    return(path.package(package))
  getNamespaceInfo(ns, "path")
}

# Create the namespace environment for a package
create_ns_env <- function(path = ".") {
  path <- pkg_path(path)
  package <- pkg_name(path)
  version <- pkg_version(path)

  if (is_loaded(package)) {
    stop("Namespace for ", package, " already exists.")
  }

  env <- makeNamespace(package, version)
  methods::setPackageName(package, env)
  # Create devtools metadata in namespace
  create_dev_meta(package)

  setNamespaceInfo(env, "path", path)
  setup_ns_imports(path)

  env
}

# This is taken directly from base::loadNamespace()
# https://github.com/wch/r-source/blob/tags/R-3-3-0/src/library/base/R/namespace.R#L235-L258
onload_assign("makeNamespace",
  eval(
    modify_lang(
      extract_lang(body(loadNamespace),

        # Find makeNamespace definition
        comp_lang, y = quote(makeNamespace <- NULL), idx = 1:2)[[3]],

      # Replace call to .Internal(registerNamespace()) is replaced by a call to
      # register_namespace
      function(x) {
        if (comp_lang(x, quote(.Internal(registerNamespace(name, env))))) {
          quote(register_namespace(name, env))
        } else {
          x
        }
      }))
)

# Read the NAMESPACE file and set up the imports metdata.
# (which is stored in .__NAMESPACE__.)
setup_ns_imports <- function(path = ".") {
  path <- pkg_path(path)
  package <- pkg_name(path)

  nsInfo <- parse_ns_file(path)
  setNamespaceInfo(package, "imports", nsInfo$imports)
}


# Read the NAMESPACE file and set up the exports metdata. This must be
# run after all the objects are loaded into the namespace because
# namespaceExport throw errors if the objects are not present.
setup_ns_exports <- function(path = ".", export_all = FALSE, export_imports = export_all) {
  path <- pkg_path(path)
  package <- pkg_name(path)

  nsInfo <- parse_ns_file(path)
  nsenv <- ns_env(package)

  if (export_all) {
    exports <- ls(nsenv, all.names = TRUE)
    # Make sure to re-export objects that are imported from other packages but
    # not copied.
    exports <- union(exports, nsInfo$exports)

    # if export_imports export all imports as well
    if (export_imports) {
      exports <- c(exports, ls(imports_env(package), all.names = TRUE))
    }

    # List of things to ignore is from loadNamespace. There are also a
    # couple things to ignore from devtools.
    ignoreidx <- exports %in% c( ".__NAMESPACE__.",
      ".__S3MethodsTable__.", ".packageName", ".First.lib", ".onLoad",
      ".onAttach", ".conflicts.OK", ".noGenerics",
      ".__DEVTOOLS__", ".cache")
    exports <- exports[!ignoreidx]

  } else {
    # This code is from base::loadNamespace
    exports <- nsInfo$exports
    for (p in nsInfo$exportPatterns)
      exports <- c(ls(nsenv, pattern = p, all.names = TRUE), exports)
  }
  # Don't try to export objects that are missing from the namespace and imports
  ns_and_imports <- c(ls(nsenv, all.names = TRUE),
                      ls(imports_env(package), all.names = TRUE))
  extra_exports <- setdiff(exports, ns_and_imports)

  if (length(extra_exports) > 0) {
    warning("Objects listed as exports, but not present in namespace: ",
            paste(extra_exports, collapse = ", "))
    exports <- intersect(ns_and_imports, exports)
  }

  # Add any S4 methods or classes, this needs to be done after checking for
  # missing exports as S4 methods with generics imported from other packages
  # are not defined in the namespace.
  exports <- add_classes_to_exports(ns = nsenv, package = package,
    exports = exports, nsInfo = nsInfo)

  # Update the exports metadata for the namespace with base::namespaceExport
  # It will throw warnings if objects are already listed in the exports
  # metadata, so catch those warnings and ignore them.
  suppressWarnings(namespaceExport(nsenv, exports))

  invisible()
}

# Lookup S4 classes for export
#
# This function uses code from base::loadNamespace. Previously this code was
# copied directly, now it is dynamically looked up instead, to prevent drift as
# base::loadNamespace changes.
onload_assign("add_classes_to_exports",
  {
    pattern <- if (getRversion() >= "4.1.0") {
      quote(if (.isMethodsDispatchOn() && hasS4m && !identical(package, "methods"))  NULL)
    } else {
      quote(if (.isMethodsDispatchOn() && .hasS4MetaData(ns) && !identical(package, "methods"))  NULL)
    }
    make_function(alist(ns =, package =, exports =, nsInfo =),
      call("{",
        quote(lev <- 0L),
        quote(hasS4m <- .hasS4MetaData(ns)),
        extract_lang(
          f = comp_lang,
          y = pattern,
          idx = c(1, 2),
          modify_lang(body(base::loadNamespace), strip_internal_calls, "methods")
        ),
        quote(exports)
      ), asNamespace("methods")
    )
  }
)

#' Parses the NAMESPACE file for a package
#'
#' @inheritParams load_all
#' @examples
#' if (has_tests()) {
#' parse_ns_file(pkgtest("testLoadHooks"))
#' }
#' @keywords internal
#' @export
parse_ns_file <- function(path = ".") {
  path <- pkg_path(path)

  parseNamespaceFile(basename(path), dirname(path),
    mustExist = FALSE)
}


# Register the S3 methods for this package
register_s3 <- function(path = ".") {
  path <- pkg_path(path)
  package <- pkg_name(path)

  nsInfo <- parse_ns_file(path)

  # Adapted from loadNamespace
  try(registerS3methods(nsInfo$S3methods, package, ns_env(package)))
}


# Reports whether a package is loaded into a namespace. It may be
# attached or not attached.
is_loaded <- function(package) {
  package %in% loadedNamespaces()
}


# Returns the namespace registry
ns_registry <- function() {
  (get(".Internal", envir = baseenv(), mode = "function"))(getNamespaceRegistry())
}
# To avoid a note about getNamespaceRegistry being missing
utils::globalVariables("getNamespaceRegistry")

# Register a namespace
register_namespace <- function(name = NULL, env = NULL) {
  # Be careful about what we allow
  if (!is.character(name) || name == "" || length(name) != 1)
    stop("'name' must be a non-empty character string.")

  if (!is.environment(env))
    stop("'env' must be an environment.")

  if (name %in% loadedNamespaces())
    stop("Namespace ", name, " is already registered.")

  # Add the environment to the registry
  nsr <- ns_registry()
  nsr[[name]] <- env

  env
}


# unregister a namespace - should be used only if unloadNamespace()
# fails for some reason
unregister_namespace <- function(name = NULL) {
  # Be careful about what we allow
  if (!is.character(name) || name == "" || length(name) != 1)
    stop("'name' must be a non-empty character string.")

  if (!(name %in% loadedNamespaces()))
    stop(name, " is not a registered namespace.")

  # Force all bindings of the namespace in case of dangling
  # references. If lazy bindings are forced after the namespace is
  # unloaded, it might lead to decompress errors if unloaded or to
  # inconsistencies if reloaded (the bindings are resolved in the new
  # namespace).
  eapply(ns_env(name), force, all.names = TRUE)

  # Remove the item from the registry
  do.call(rm, args = list(name, envir = ns_registry()))
  invisible()
}

unregister_methods <- function(package) {
  # Unloading S3 methods manually avoids lazy-load errors when the new
  # package is loaded overtop the old one. It also prevents removed
  # methods from staying registered.
  s3_unregister(package)

  # S4 classes that were created by the package need to be removed in a special way.
  remove_s4_classes(package)
}
