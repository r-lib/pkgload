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
#' of the laded package. It presently adds a replacement version of
#' `system.file` which returns different paths from
#' `base::system.file`. This is needed because installed and uninstalled
#' package sources have different directory structures. Note that this is not
#' a perfect replacement for `base::system.file`.
#'
#' @param pkg package description, can be path or package name.  See
#'   [as.package()] for more information.
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
#' @param helpers if \code{TRUE} loads \pkg{testthat} test helpers.
#' @param quiet if `TRUE` suppresses output from this function.
#' @param recollate if `TRUE`, run [roxygen2::update_collate()] before loading.
#' @inheritParams as.package
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
load_all <- function(pkg = ".", reset = TRUE, recompile = FALSE,
                     export_all = TRUE, helpers = TRUE, recollate = FALSE, quiet = FALSE) {
  pkg <- as.package(pkg)

  if (!quiet) message("Loading ", pkg$package)

  if (pkg$package == "compiler") {
    # Disable JIT while loading the compiler package to avoid interference
    # (otherwise the compiler package would be loaded as a side effect of
    # JIT compilation and it would be locked before we can insert shims into
    # it).
    oldEnabled <- compiler::enableJIT(0)
    on.exit(compiler::enableJIT(oldEnabled), TRUE)
  }

  if (isTRUE(recollate)) {
    check_suggested("roxygen2")
    roxygen2::update_collate(pkg$path)
    # Refresh the pkg structure with any updates to the Collate entry
    # in the DESCRIPTION file
  }
  pkg$collate <- as.package(pkg$path)$collate

  # Forcing all of the promises for the loaded namespace now will avoid lazy-load
  # errors when the new package is loaded overtop the old one.
  #
  # Reloading devtools is a special case. Normally, objects in the
  # namespace become inaccessible if the namespace is unloaded before the
  # object has been accessed. Instead we force the object so they will still be
  # accessible.
  if (is_loaded(pkg)) {
    eapply(ns_env(pkg), force, all.names = TRUE)
  }

  # Check description file is ok
  check <- ("tools" %:::% ".check_package_description")(
    file.path(pkg$path, "DESCRIPTION"))
  if (length(check) > 0) {
    msg <- utils::capture.output(("tools" %:::% "print.check_package_description")(check))
    message("Invalid DESCRIPTION:\n", paste(msg, collapse = "\n"))
  }

  ## The unload() has to come before unload_dll(), for packages with
  ## compiled code, becauase they might crash of objects still use the
  ## DLL's memory.
  if (reset) {
    clear_cache()
    if (is_loaded(pkg)) unload(pkg, quiet = quiet)
  }

  if (is_loaded(pkg) && is.null(dev_meta(pkg$package))) {
    # If installed version of package loaded, unload it
    # (and also the DLLs)
    unload(pkg, quiet = quiet)
  } else {
    # Unload only DLLs
    unload_dll(pkg)
  }

  if (recompile) {
    pkgbuild::clean_dll(pkg$path)
  }

  # Compile dll if it exists
  pkgbuild::compile_dll(pkg$path, quiet = quiet)

  # If installed version of package loaded, unload it, again
  # (needed for dependencies of pkgbuild)
  if (is_loaded(pkg) && is.null(dev_meta(pkg$package))) {
    unload(pkg, quiet = TRUE)
  }

  # Set up the namespace environment ----------------------------------
  # This mimics the procedure in loadNamespace

  if (!is_loaded(pkg)) create_ns_env(pkg)

  out <- list(env = ns_env(pkg))

  # Load dependencies
  load_depends(pkg)
  load_imports(pkg)
  # Add shim objects to imports environment
  insert_imports_shims(pkg)

  out$data <- load_data(pkg)

  out$code <- load_code(pkg)
  register_s3(pkg)
  out$dll <- load_dll(pkg)

  # Run namespace load hooks
  run_pkg_hook(pkg, "load")
  run_ns_load_actions(pkg)
  run_user_hook(pkg, "load")

  # Set up the exports in the namespace metadata (this must happen after
  # the objects are loaded)
  setup_ns_exports(pkg, export_all)

  # Set up the package environment ------------------------------------
  # Create the package environment if needed
  if (!is_attached(pkg)) attach_ns(pkg)

  # Copy over objects from the namespace environment
  export_ns(pkg)

  # Run hooks
  run_pkg_hook(pkg, "attach")
  run_user_hook(pkg, "attach")

  # Source test helpers into package environment
  if (uses_testthat(pkg) && helpers) {
    withr::with_envvar(c(NOT_CRAN = "true", DEVTOOLS_LOAD = "true"),
      testthat::source_test_helpers(find_test_dir(pkg$path), env = pkg_env(pkg))
    )
  }


  # Replace help and ? in utils package environment
  insert_global_shims()

  invisible(out)
}


uses_testthat <- function(pkg = ".") {
  pkg <- as.package(pkg)

  paths <- c(
    file.path(pkg$path, "inst", "tests"),
    file.path(pkg$path, "tests", "testthat")
  )

  any(dir.exists(paths))
}

find_test_dir <- function(path) {
  testthat <- file.path(path, "tests", "testthat")
  if (dir.exists(testthat)) return(testthat)

  inst <- file.path(path, "inst", "tests")
  if (dir.exists(inst)) return(inst)

  stop("No testthat directories found in ", path, call. = FALSE)
}
