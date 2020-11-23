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
#'     you can access them interactively. devtools sets the
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
#' @param compile If `TRUE` always recompiles the package; if `NA`
#'   recompiles if needed (as determined by [pkgbuild::needs_compile()]);
#'   if `FALSE`, never recompiles.
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
#' @param recompile DEPRECATED. force a recompile of DLL from source code, if
#'   present. This is equivalent to running [pkgbuild::clean_dll()] before
#'   `load_all`
#' @param warn_conflicts If `TRUE`, issue a warning if there are conflicts
#'   between the exported functions and functions in the global namespace. This
#'   most commonly happens when you accidently source an R file rather than using
#'   `load_all()`, or define a function directly in the R console, and can be
#'   frustrating to debug.
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
load_all <- function(path = ".", reset = TRUE, compile = NA,
                     export_all = TRUE, export_imports = export_all,
                     helpers = TRUE, attach_testthat = uses_testthat(path),
                     quiet = FALSE, recompile = FALSE, warn_conflicts = TRUE) {
  path <- pkg_path(path)
  package <- pkg_name(path)
  description <- pkg_desc(path)

  if (!quiet) cli::cli_alert_info("Loading {.pkg {package}}")

  if (package == "compiler") {
    # Disable JIT while loading the compiler package to avoid interference
    # (otherwise the compiler package would be loaded as a side effect of
    # JIT compilation and it would be locked before we can insert shims into
    # it).
    oldEnabled <- compiler::enableJIT(0)
    on.exit(compiler::enableJIT(oldEnabled), TRUE)
  }

  # Check description file is ok
  check <- ("tools" %:::% ".check_package_description")(
    package_file("DESCRIPTION", path = path))

  if (length(check) > 0) {
    msg <- utils::capture.output(("tools" %:::% "print.check_package_description")(check))
    cli::cli_alert_danger("Invalid DESCRIPTION")
    cli::cli_code(msg)
  }

  # Compile dll if requested
  if (missing(compile) && !missing(recompile)) {
    compile <- if (isTRUE(recompile)) TRUE else NA
  }

  if (isTRUE(compile)) {
    pkgbuild::compile_dll(path, force = TRUE, quiet = quiet)
  } else if (identical(compile, NA)) {
    pkgbuild::compile_dll(path, quiet = quiet)
  } else if (identical(compile, FALSE)) {
    # don't compile
  } else {
    stop("`compile` must be a logical vector of length 1", call. = FALSE)
  }

  if (reset) {
    clear_cache()

    # Remove package from known namespaces. We don't unload it to allow
    # safe usage of dangling references.
    if (is_loaded(package)) {
      unload_pkg_env(package)
      unregister_methods(package)
      unregister_namespace(package)
    }
  }

  if (!is_loaded(package)) {
    create_ns_env(path)
  }

  out <- list(env = ns_env(package))

  # Load dependencies
  load_depends(path)
  load_imports(path)
  # Add shim objects to imports environment
  insert_imports_shims(package)

  out$data <- load_data(path)

  out$code <- load_code(path)
  register_s3(path)
  if (identical(compile, FALSE)) {
    out$dll <- try_load_dll(path)
  } else {
    out$dll <- load_dll(path)
  }

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

  # Source test helpers into package environment
  if (uses_testthat(path) && helpers) {
    withr_with_envvar(c(NOT_CRAN = "true", DEVTOOLS_LOAD = "true"),
      testthat_source_test_helpers(find_test_dir(path), env = pkg_env(package))
    )
  }

  # Replace help and ? in utils package environment
  insert_global_shims()

  if (isTRUE(warn_conflicts)) {
    warn_if_conflicts(package, getNamespaceExports(out$env), names(globalenv()))
  }

  invisible(out)
}

warn_if_conflicts <- function(package, nms1, nms2) {
  both <- sort(intersect(nms1, nms2))
  if (length(both) == 0) {
    return(invisible())
  }

  header <- cli::rule(
    left = crayon::bold("Conflicts"),
    right = paste0(package, " ", "conflicts")
  )

  bullets <- conflict_bullets(package, both)

  directions <- crayon::silver(
    paste0(
      "Did you accidentally source a file rather than using `load_all()`?\n",
      "Run `rm(list = c(", paste0('"', both, '"', collapse = ", "),
      "))` to remove the conflicts."
    )
  )

  rlang::warn(
    sprintf(
      "\n%s\n%s\n\n%s",
      header,
      bullets,
      directions
    ),
    .subclass = "pkgload::conflict"
  )
}

conflict_bullets <- function(package, both) {
  # Show three bullets plus ellipsis if more than four bullets.
  # The output size is limited, also the bullets are vers repetitive.
  MAX_BULLETS <- 3

  if (length(both) > MAX_BULLETS + 1) {
    more <- paste0(
      "\n", cli::symbol$ellipsis, " and ",
      length(both) - MAX_BULLETS, " more"
    )
    both <- utils::head(both, MAX_BULLETS)
  } else {
    more <- ""
  }

  bullets <- paste0(collapse = "\n",
    sprintf(
      "%s %s masks %s::%s()",
      crayon::red(cli::symbol$cross),
      format(crayon::green(paste0(both, "()"))),
      crayon::blue(package),
      both
    )
  )

  paste0(bullets, more)
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
