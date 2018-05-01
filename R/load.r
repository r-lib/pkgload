#' Load complete package.
#'
#' `load_all` loads a package. It roughly simulates what happens
#' when a package is installed and loaded with [library()].
#'
#' Currently `load_all`:
#'
#' \itemize{
#'   \item Loads all data files in `data/`.  See [load_data()]
#'     for more details.
#'
#'   \item Sources all R files in the R directory, storing results in
#'     environment that behaves like a regular package namespace. See
#'     below and [load_code()] for more details.
#'
#'   \item Compiles any C, C++, or Fortran code in the `src/` directory
#'     and connects the generated DLL into R. See [compile_dll()]
#'     for more details.
#'
#'   \item Runs `.onAttach()`, `.onLoad()` and `.onUnload()`
#'     functions at the correct times.
#'
#'   \item If you use \pkg{testthat}, will load all test helpers so
#'     you can access them interactively. Devtools sets the
#'     \code{DEVTOOLS_LOAD} environment variable to \code{"true"} to
#'     let you check whether the helpers are run during package loading.
#'
#' }
#'
#' @section Namespaces:
#' The namespace environment `<namespace:pkgname>`, is a child of
#' the imports environment, which has the name attribute
#' `imports:pkgname`. It is in turn is a child of
#' `<namespace:base>`, which is a child of the global environment.
#' (There is also a copy of the base namespace that is a child of the empty
#' environment.)
#'
#' The package environment `<package:pkgname>` is an ancestor of the
#' global environment. Normally when loading a package, the objects
#' listed as exports in the NAMESPACE file are copied from the namespace
#' to the package environment. However, `load_all` by default will
#' copy all objects (not just the ones listed as exports) to the package
#' environment. This is useful during development because it makes all
#' objects easy to access.
#'
#' To export only the objects listed as exports, use
#' `export_all=FALSE`. This more closely simulates behavior when
#' loading an installed package with [library()], and can be
#' useful for checking for missing exports.
#'
#' @section Shim files:
#' `load_all` also inserts shim functions into the imports environment
#' of the loaded package. It presently adds a replacement version of
#' `system.file` which returns different paths from
#' `base::system.file`. This is needed because installed and uninstalled
#' package sources have different directory structures. Note that this is not
#' a perfect replacement for `base::system.file`.
#'
#' @param path Path to a package, or within a package.
#' @param reset clear package environment and reset file cache before loading
#'   any pieces of the package. This is equivalent to running
#'   [unload()] and is the default. Use `reset = FALSE` may be
#'   faster for large code bases, but is a significantly less accurate
#'   approximation.
#' @param recompile force a recompile of DLL from source code, if present.
#'   This is equivalent to running [pkgbuild::clean_dll()] before
#'   `load_all`
#' @param export_all If `TRUE` (the default), export all objects.
#'   If `FALSE`, export only the objects that are listed as exports
#'   in the NAMESPACE file.
#' @param export_imports If `TRUE` (the default), export all objects that are
#'   imported by the package. If `FALSE` export only objects defined in the
#'   package.
#' @param attach_testthat If `TRUE`, attach \pkg{testthat} to the search path,
#'   which more closely mimics the environment within test files.
#' @param helpers if \code{TRUE} loads \pkg{testthat} test helpers.
#' @param quiet if `TRUE` suppresses output from this function.
#' @keywords programming
#' @examples
#' \dontrun{
#' # Load the package in the current directory
#' load_all("./")
#'
#' # Running again loads changed files
#' load_all("./")
#'
#' # With reset=TRUE, unload and reload the package for a clean start
#' load_all("./", TRUE)
#'
#' # With export_all=FALSE, only objects listed as exports in NAMESPACE
#' # are exported
#' load_all("./", export_all = FALSE)
#' }
#' @export
load_all <- function(path = ".", reset = TRUE, recompile = FALSE,
                     export_all = TRUE, export_imports = export_all,
                     helpers = TRUE, attach_testthat = uses_testthat(path),
                     quiet = FALSE) {
  path <- pkg_path(path)
  package <- pkg_name(path)
  description <- pkg_desc(path)

  if (!quiet) message("Loading ", package)

  if (package == "compiler") {
    # Disable JIT while loading the compiler package to avoid interference
    # (otherwise the compiler package would be loaded as a side effect of
    # JIT compilation and it would be locked before we can insert shims into
    # it).
    oldEnabled <- compiler::enableJIT(0)
    on.exit(compiler::enableJIT(oldEnabled), TRUE)
  }

  # Forcing all of the promises for the loaded namespace now will avoid lazy-load
  # errors when the new package is loaded overtop the old one.
  #
  # Reloading devtools is a special case. Normally, objects in the
  # namespace become inaccessible if the namespace is unloaded before the
  # object has been accessed. Instead we force the object so they will still be
  # accessible.
  if (is_loaded(package)) {
    eapply(ns_env(package), force, all.names = TRUE)
  }

  # Check description file is ok
  check <- ("tools" %:::% ".check_package_description")(
    package_file("DESCRIPTION", path = path))

  if (length(check) > 0) {
    msg <- utils::capture.output(("tools" %:::% "print.check_package_description")(check))
    message("Invalid DESCRIPTION:\n", paste(msg, collapse = "\n"))
  }

  ## The unload() has to come before unload_dll(), for packages with
  ## compiled code, becauase they might crash of objects still use the
  ## DLL's memory.
  if (reset) {
    clear_cache()
    if (is_loaded(package)) unload(package, quiet = quiet)
  }

  if (is_loaded(package) && is.null(dev_meta(package))) {
    # If installed version of package loaded, unload it
    # (and also the DLLs)
    unload(package, quiet = quiet)
  } else {
    # Unload only DLLs
    unload_dll(package)
  }

  if (recompile) {
    pkgbuild::clean_dll(path)
  }

  # Compile dll if it exists
  pkgbuild::compile_dll(path, quiet = quiet)

  # If installed version of package loaded, unload it, again
  # (needed for dependencies of pkgbuild)
  if (is_loaded(package) && is.null(dev_meta(package))) {
    unload(package, quiet = quiet)
  }

  # Set up the namespace environment ----------------------------------
  # This mimics the procedure in loadNamespace

  if (!is_loaded(package)) create_ns_env(path)

  out <- list(env = ns_env(package))

  # Load dependencies
  load_depends(path)
  load_imports(path)
  # Add shim objects to imports environment
  insert_imports_shims(package)

  out$data <- load_data(path)

  out$code <- load_code(path)
  register_s3(path)
  out$dll <- load_dll(path)

  # attach testthat to the search path
  if (isTRUE(attach_testthat) && package != "testthat") {
    ("base" %:::% "library")("testthat")
  }

  # Run namespace load hooks
  run_pkg_hook(package, "load")
  run_ns_load_actions(package)
  run_user_hook(package, "load")

  # Set up the package environment ------------------------------------
  # Create the package environment if needed
  if (!is_attached(package)) attach_ns(package)

  # Copy over lazy data objects from the namespace environment
  export_lazydata(package)

  # Source test helpers into package environment
  if (uses_testthat(path) && helpers) {
    withr_with_envvar(c(NOT_CRAN = "true", DEVTOOLS_LOAD = "true"),
      testthat_source_test_helpers(find_test_dir(path), env = ns_env(package))
    )
  }

  # Set up the exports in the namespace metadata (this must happen after
  # the objects are loaded)
  setup_ns_exports(path, export_all, export_imports)

  # Copy over objects from the namespace environment
  export_ns(package)

  # Assign .Depends, if any, to package environment from namespace
  assign_depends(package)

  # Run hooks
  run_pkg_hook(package, "attach")
  run_user_hook(package, "attach")

  # Replace help and ? in utils package environment
  insert_global_shims()

  # Propagate new definitions to namespace imports of loaded packages.
  propagate_ns(package)

  invisible(out)
}


uses_testthat <- function(path = ".") {
  paths <- c(
    package_file("inst", "tests", path = path),
    package_file("tests", "testthat", path = path)
  )

  any(dir.exists(paths))
}

find_test_dir <- function(path) {
  testthat <- package_file("tests", "testthat", path = path)
  if (dir.exists(testthat)) return(testthat)

  inst <- package_file("inst", "tests", path = path)
  if (dir.exists(inst)) return(inst)

  stop("No testthat directories found in ", path, call. = FALSE)
}

propagate_ns <- function(package) {
  for (ns in loadedNamespaces()) {
    imports <- getNamespaceImports(ns)
    if (package %in% names(imports)) {
      env <- ns_env(ns)
      lapply(ls(env, all.names = TRUE), unlockBinding, env)

      imp <- imports_env(ns)
      lapply(ls(imp, all.names = TRUE), unlockBinding, imp)

      unlock_environment(env)
      unlock_environment(imp)
      update_imports(ns)
      lockEnvironment(env)
      lockEnvironment(imp)
    }
  }
}
