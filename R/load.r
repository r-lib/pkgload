#' Load complete package.
#'
#' \code{load_all} loads a package. It roughly simulates what happens
#' when a package is installed and loaded with \code{\link{library}}.
#'
#' Currently \code{load_all}:
#'
#' \itemize{
#'   \item Loads all data files in \code{data/}.  See \code{\link{load_data}}
#'     for more details.
#'
#'   \item Sources all R files in the R directory, storing results in
#'     environment that behaves like a regular package namespace. See
#'     below and \code{\link{load_code}} for more details.
#'
#'   \item Compiles any C, C++, or Fortran code in the \code{src/} directory
#'     and connects the generated DLL into R. See \code{\link{compile_dll}}
#'     for more details.
#'
#'   \item Runs \code{.onAttach()}, \code{.onLoad()} and \code{.onUnload()}
#'     functions at the correct times.
#'
#'   \item If you use \pkg{testthat}, will load all test helpers so you
#'     can access them interactively.
#'
#' }
#'
#' @section Namespaces:
#' The namespace environment \code{<namespace:pkgname>}, is a child of
#' the imports environment, which has the name attribute
#' \code{imports:pkgname}. It is in turn is a child of
#' \code{<namespace:base>}, which is a child of the global environment.
#' (There is also a copy of the base namespace that is a child of the empty
#' environment.)
#'
#' The package environment \code{<package:pkgname>} is an ancestor of the
#' global environment. Normally when loading a package, the objects
#' listed as exports in the NAMESPACE file are copied from the namespace
#' to the package environment. However, \code{load_all} by default will
#' copy all objects (not just the ones listed as exports) to the package
#' environment. This is useful during development because it makes all
#' objects easy to access.
#'
#' To export only the objects listed as exports, use
#' \code{export_all=FALSE}. This more closely simulates behavior when
#' loading an installed package with \code{\link{library}}, and can be
#' useful for checking for missing exports.
#'
#' @section Shim files:
#' \code{load_all} also inserts shim functions into the imports environment
#' of the laded package. It presently adds a replacement version of
#' \code{system.file} which returns different paths from
#' \code{base::system.file}. This is needed because installed and uninstalled
#' package sources have different directory structures. Note that this is not
#' a perfect replacement for \code{base::system.file}.
#'
#' @param pkg package description, can be path or package name.  See
#'   \code{\link{as.package}} for more information.
#' @param reset clear package environment and reset file cache before loading
#'   any pieces of the package. This is equivalent to running
#'   \code{\link{unload}} and is the default. Use \code{reset = FALSE} may be
#'   faster for large code bases, but is a significantly less accurate
#'   approximation.
#' @param recompile force a recompile of DLL from source code, if present.
#'   This is equivalent to running \code{\link{clean_dll}} before
#'   \code{load_all}
#' @param export_all If \code{TRUE} (the default), export all objects.
#'   If \code{FALSE}, export only the objects that are listed as exports
#'   in the NAMESPACE file.
#' @param quiet if \code{TRUE} suppresses output from this function.
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
                     export_all = TRUE, quiet = FALSE) {
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

  roxygen2::update_collate(pkg$path)
  # Refresh the pkg structure with any updates to the Collate entry
  # in the DESCRIPTION file
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

  # If installed version of package loaded, unload it
  if (is_loaded(pkg) && is.null(dev_meta(pkg$package))) {
    unload(pkg)
  }

  # Unload dlls
  unload_dll(pkg)

  if (reset) {
    clear_cache()
    if (is_loaded(pkg)) unload(pkg)
  }

  if (recompile) clean_dll(pkg)

  # Compile dll if it exists
  compile_dll(pkg, quiet = quiet)


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
  if (uses_testthat(pkg)) {
    testthat::source_test_helpers(find_test_dir(pkg$path), env = pkg_env(pkg))
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
