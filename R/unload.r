#' Unload a package
#'
#' @description
#'
#' `unload()` attempts to cleanly unload a package, including unloading
#' its namespace, deleting S4 class definitions and unloading any loaded
#' DLLs. Unfortunately S4 classes are not really designed to be cleanly
#' unloaded, and so we have to manually modify the class dependency graph in
#' order for it to work - this works on the cases for which we have tested
#' but there may be others.  Similarly, automated DLL unloading is best tested
#' for simple scenarios (particularly with `useDynLib(pkgname)` and may
#' fail in other cases. If you do encounter a failure, please file a bug report
#' at \url{https://github.com/r-lib/pkgload/issues}.
#'
#' `unregister()` is a gentler version of `unload()` which removes the
#' package from the search path, unregisters methods, and unregisters
#' the namespace. It doesn't unload the namespace or its DLL to keep
#' it in working order in case of dangling references.
#'
#' @inheritParams ns_env
#' @param quiet if `TRUE` suppresses output from this function.
#'
#' @examples
#' \dontrun{
#' # Unload package that is in current directory
#' unload()
#'
#' # Unload package that is in ./ggplot2/
#' unload(pkg_name("ggplot2/"))
#'
#' library(ggplot2)
#' # unload the ggplot2 package directly by name
#' unload("ggplot2")
#' }
#' @export
unload <- function(package = pkg_name(), quiet = FALSE) {

  if (package == "compiler") {
    # Disable JIT compilation as it could interfere with the compiler
    # unloading. Also, if the JIT was kept enabled, it would cause the
    # compiler package to be loaded again soon, anyway. Note if we
    # restored the JIT level after the unloading, the call to
    # enableJIT itself would load the compiler again.
    oldEnable <- compiler::enableJIT(0)
    if (oldEnable != 0) {
      warning("JIT automatically disabled when unloading the compiler.")
    }
  }

  if (!package %in% loadedNamespaces()) {
    stop("Package ", package, " not found in loaded packages or namespaces")
  }

  unregister_methods(package)

  # unloadNamespace calls onUnload hook and .onUnload, and detaches the
  # package if it's attached. It will fail if a loaded package needs it.
  unloaded <- tryCatch({
    unloadNamespace(package)
    TRUE
  }, error = function(e) FALSE)

  if (!unloaded) {
    # unloadNamespace() failed before we get to the detach, so need to
    # manually detach
    unload_pkg_env(package)

    # Can't use loadedNamespaces() and unloadNamespace() here because
    # things can be in a weird state.
    unregister_namespace(package)
  }

  # Clear so that loading the package again will re-read all files
  clear_cache()

  # Do this after detach, so that packages that have an .onUnload function
  # which unloads DLLs (like MASS) won't try to unload the DLL twice.
  unload_dll(package)
}

#' @rdname unload
#' @export
unregister <- function(package = pkg_name()) {
  unload_pkg_env(package)
  unregister_methods(package)
  unregister_namespace(package)
}

unload_pkg_env <- function(package) {
  if (is_attached(package)) {
    pos <- which(pkg_env_name(package) == search())
    suppressWarnings(detach(pos = pos, force = TRUE))
  }
}

# This unloads dlls loaded by either library() or load_all()
unload_dll <- function(package) {
  # Always run garbage collector to force any deleted external pointers to
  # finalise
  gc()

  # Special case for devtools - don't unload DLL because we need to be able
  # to access nsreg() in the DLL in order to run makeNamespace. This means
  # that changes to compiled code in devtools can't be reloaded with
  # load_all -- it requires a reinstallation.
  if (package == "pkgload") {
    return(invisible())
  }

  pkglibs <- loaded_dlls(package)

  for (lib in pkglibs) {
    dyn.unload(lib[["path"]])
  }

  # Remove the unloaded dlls from .dynLibs()
  libs <- .dynLibs()
  .dynLibs(libs[!(libs %in% pkglibs)])

  invisible()
}

s3_unregister <- function(package) {
  ns <- ns_env(package)

  # If the package is loaded, but not installed this will fail, so we bail out in that case.
  ns_defs <- suppressWarnings(try(parse_ns_file(system.file(package = package)), silent = TRUE))
  if (inherits(ns_defs, "try-error")) {
    return()
  }
  methods <- ns_defs$S3methods[, 1:2, drop = FALSE]

  for (i in seq_len(nrow(methods))) {
    method <- methods[i, , drop = FALSE]

    generic <- env_get(ns, method[[1]], inherit = TRUE, default = NULL)
    if (is_null(generic)) {
      next
    }

    generic_ns <- topenv(fn_env(generic))
    if (!is_namespace(generic_ns)) {
      next
    }

    # Don't remove methods for generics defined in the namespace being
    # unloaded. The stale namespace should still work as much as
    # possible.
    if (is_string(ns_env_name(generic_ns), package)) {
      next
    }

    table <- generic_ns$.__S3MethodsTable__.
    if (!is_environment(table)) {
      next
    }

    nm <- paste0(method, collapse = ".")
    env_unbind(table, nm)
  }
}
