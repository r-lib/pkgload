#' Load complete package
#'
#' @description
#' `load_all()` loads a package. It roughly simulates what happens
#' when a package is installed and loaded with [library()], without
#' having to first install the package. It:
#'
#' - Loads all data files in `data/`. See [load_data()] for more
#'   details.
#'
#' - Sources all R files in the R directory, storing results in
#'   environment that behaves like a regular package namespace. See
#'   [load_code()] for more details.
#'
#' - Adds a shim from [system.file()] to [shim_system.file()] in
#'   the imports environment of the package. This ensures that `system.file()`
#'   works with both development and installed packages despite their differing
#'   directory structures.
#'
#' - Adds shims from `help()` and `?` to [shim_help()] and [shim_question()]
#'   to make it easier to preview development documentation.
#'
#' - Compiles any C, C++, or Fortran code in the `src/` directory and
#'   connects the generated DLL into R. See [pkgbuild::compile_dll()]
#'   for more details.
#'
#' - Loads any compiled translations in `inst/po`.
#'
#' - Runs `.onAttach()`, `.onLoad()` and `.onUnload()` functions at
#'   the correct times.
#'
#' - If you use \pkg{testthat}, will load all test helpers so you can
#'   access them interactively. devtools sets the `DEVTOOLS_LOAD`
#'   environment variable to the package name to let you check whether the
#'   helpers are run during package loading.
#'
#' `is_loading()` returns `TRUE` when it is called while `load_all()`
#' is running. This may be useful e.g. in `.onLoad` hooks.
#' A package loaded with `load_all()` can be identified with
#' [is_dev_package()].
#'
#' # Differences to regular loading
#'
#' `load_all()` tries its best to reproduce the behaviour of
#' [loadNamespace()] and [library()]. However it deviates from normal
#' package loading in several ways.
#'
#' -  `load_all()` doesn't install the package to a library, so [system.file()]
#'    doesn't work. pkgload fixes this for the package itself installing a shim,
#'    [shim_system.file()]. However, this shim is not visible to third party
#'    packages, so they will fail if they attempt to find files within your
#'    package. One potential workaround is to use [fs::path_package()]
#'    instead of `system.file()`, since that understands the mechanisms that
#'    devtools uses to load packages.
#'
#' -  `load_all()` loads all packages referenced in `Imports` at load time,
#'    but `loadNamespace()` and `library()` only load package dependencies as
#'    they are needed.
#'
#' - `load_all()` copies all objects (not just the ones listed as exports)
#'    into the package environment. This is useful during development because
#'    it makes internal objects easy to access.  To export only the objects
#'    listed as exports, use `export_all = FALSE`. This more closely simulates
#'    behavior when loading an installed package with [library()], and can
#'    be useful for checking for missing exports.
#'
#' # Controlling the debug compiler flags
#'
#' `load_all()` delegates to [pkgbuild::compile_dll()] to perform the actual
#' compilation, during which by default some debug compiler flags are
#' appended. If you would like to produce an optimized build instead,
#' you can opt out by setting the `pkg.build_extra_flags`
#' option or the `PKG_BUILD_EXTRA_FLAGS` environment variable to `FALSE`.
#' For further details see the Details section in [pkgbuild::compile_dll()].
#'
#'
#' @param path Path to a package, or within a package.
#' @param reset `r lifecycle::badge("deprecated")` This is no longer supported
#'   because preserving the namespace requires unlocking its environment, which
#'   is no longer possible in recent versions of R.
#' @param compile If `TRUE` always recompiles the package; if `NA`
#'   recompiles if needed (as determined by [pkgbuild::needs_compile()]);
#'   if `FALSE`, never recompiles.
#' @param attach Whether to attach a package environment to the search
#'   path. If `FALSE` `load_all()` behaves like `loadNamespace()`. If
#'   `TRUE` (the default), it behaves like `library()`. If `FALSE`,
#'   the `export_all`, `export_imports`, and `helpers` arguments have
#'   no effect.
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
#'   `load_all()`
#' @param warn_conflicts If `TRUE`, issues a warning if a function in the global
#'   environment masks a function in the package. This can happen when you
#'   accidentally source a `.R` file, rather than using `load_all()`, or if you
#'   define a function directly in the R console. This is frustrating to debug,
#'   as it feels like the changes you make to the package source aren't having
#'   the expected effect.
#' @keywords programming
#' @examples
#' \dontrun{
#' # Load the package in the current directory
#' load_all("./")
#'
#' # Running again loads changed files
#' load_all("./")
#'
#' # With export_all=FALSE, only objects listed as exports in NAMESPACE
#' # are exported
#' load_all("./", export_all = FALSE)
#' }
#' @export
load_all <- function(
  path = ".",
  reset = TRUE,
  compile = NA,
  attach = TRUE,
  export_all = TRUE,
  export_imports = export_all,
  helpers = export_all,
  attach_testthat = uses_testthat(path),
  quiet = NULL,
  recompile = FALSE,
  warn_conflicts = TRUE
) {
  if (!isTRUE(reset)) {
    lifecycle::deprecate_warn(
      when = "1.3.5",
      what = "load_all(reset)",
      details = "`reset = FALSE` is no longer supported."
    )
  }

  path <- pkg_path(path)
  package <- pkg_name(path)
  description <- pkg_desc(path)

  local_envvar(DEVTOOLS_LOAD = package)

  quiet <- load_all_quiet(quiet, "load_all")

  if (!quiet) {
    cli::cli_inform(c("i" = "Loading {.pkg {package}}"))
  }

  if (package == "compiler") {
    # Disable JIT while loading the compiler package to avoid interference
    # (otherwise the compiler package would be loaded as a side effect of
    # JIT compilation and it would be locked before we can insert shims into
    # it).
    oldEnabled <- compiler::enableJIT(0)
    defer(compiler::enableJIT(oldEnabled))
  }

  if (missing(compile) && !missing(recompile)) {
    compile <- if (isTRUE(recompile)) TRUE else NA
  }

  if (isTRUE(compile)) {
    pkgbuild::clean_dll(path)
    pkgbuild::compile_dll(path, quiet = quiet)
  } else if (identical(compile, NA)) {
    pkgbuild::compile_dll(path, quiet = quiet)
  } else if (identical(compile, FALSE)) {
    # don't compile
  } else {
    cli::cli_abort("{.arg compile} must be a logical vector of length 1.")
  }

  old_methods <- list()
  clear_cache()

  # Remove package from known namespaces. We don't unload it to allow
  # safe usage of dangling references.
  if (is_loaded(package)) {
    patch_colon(package)

    methods_env <- ns_s3_methods(package)
    unregister(package)

    # Save foreign methods after unregistering the package's own
    # methods. We'll restore the foreign methods but let the package
    # register its own methods again.
    old_methods <- as.list(methods_env)
    old_methods <- Filter(
      function(x) is_foreign_method(x, package),
      old_methods
    )
  }

  create_ns_env(path)

  out <- list(env = ns_env(package))

  # Load dependencies
  load_depends(path, quiet = quiet)
  load_imports(path)
  # Add shim objects to imports environment
  insert_imports_shims(package)

  out$data <- load_data(path)

  out$code <- load_code(path, quiet = quiet)
  register_s3(path)
  if (identical(compile, FALSE)) {
    out$dll <- try_load_dll(path)
  } else {
    out$dll <- load_dll(path)
  }

  # attach testthat to the search path
  if (isTRUE(attach_testthat) && package != "testthat") {
    ("base" %:::% "library")("testthat", warn.conflicts = FALSE)
  }

  load_po(package, path)

  # Run namespace load hooks
  run_pkg_hook(package, "load")

  # Set up the exports in the namespace metadata (this must happen after
  # the objects are loaded)
  setup_ns_exports(path)

  run_ns_load_actions(package)

  ns <- ns_env(package)
  lockEnvironment(ns)
  for (nm in names(ns)) {
    lockBinding(nm, ns)
  }

  run_user_hook(package, "load")

  # Set up the package environment ------------------------------------
  # Create the package environment if needed
  if (attach) {
    setup_pkg_env(package)
  }

  env_bind(ns_s3_methods(package), !!!old_methods)

  if (attach) {
    run_pkg_hook(package, "attach")
    run_user_hook(package, "attach")
    populate_pkg_env(package, path, export_all, export_imports, helpers)
  }

  # Replace help and ? in utils package environment
  insert_global_shims()

  if (isTRUE(warn_conflicts)) {
    warn_if_conflicts(
      package,
      out$env,
      globalenv()
    )
  }

  # Regenerate compilation database if needed. This must happen after loading
  # the DLL to ensure that `Makevars` is up-to-date.
  catch_and_warn(
    if (has_compilation_db(description)) {
      generate_db(path)
    }
  )

  invisible(out)
}

# The override logic is somewhat complicated but allows changing the
# global verbosity default to `FALSE` during unit tests, as well as
# turning on or off verbosity selectively in different loading phases.
load_all_quiet <- function(quiet, fn = NULL) {
  if (!is_null(fn)) {
    # This overwrites the `quiet` setting
    quiet <- peek_option(sprintf("testthat:::%s_quiet_override", fn)) %||% quiet
  }

  # This doesn't overwrite the `quiet` setting, only changes the default
  quiet %||% peek_option("testthat:::load_all_quiet_default") %||% FALSE
}

is_function_in_environment <- function(name, env) {
  vapply(
    name,
    exists,
    logical(1),
    where = env,
    mode = "function",
    inherits = FALSE
  )
}

warn_if_conflicts <- function(package, env1, env2) {
  nms1 <- get_exports(env1)
  nms2 <- get_exports(env2)

  both <- sort(intersect(nms1, nms2))

  # Verify are functions in both environments
  both <- both[
    is_function_in_environment(both, env1) &
      is_function_in_environment(both, env2)
  ]

  if (length(both) == 0) {
    return(invisible())
  }

  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = paste0(package, " ", "conflicts")
  )

  bullets <- conflict_bullets(package, both)

  objects <- paste0('"', both, '"', collapse = ", ")
  run_rm <- sprintf("rm(list = c(%s))", objects)
  run_rm <- style_hyperlink_run(run_rm)

  directions <- c(
    "i" = cli::col_silver(
      "Did you accidentally source a file rather than using `load_all()`?"
    ),
    " " = cli::col_silver("Run {run_rm} to remove the conflicts.")
  )

  cli::cli_warn(
    c(header, bullets, directions),
    class = "pkgload::conflict"
  )
}

get_exports <- function(ns) {
  if (isNamespace(ns)) {
    nms <- getNamespaceExports(ns)
  } else {
    nms <- names(ns)
  }
  nms
}

conflict_bullets <- function(package, both) {
  # Show three bullets plus ellipsis if more than four bullets.
  # The output size is limited, also the bullets are vers repetitive.
  MAX_BULLETS <- 3

  if (length(both) > MAX_BULLETS + 1) {
    more <- c(" " = glue::glue("{cli::symbol$ellipsis} and more."))
    both <- utils::head(both, MAX_BULLETS)
  } else {
    more <- NULL
  }

  bullets <- glue::glue(
    "`{cli::col_green(both)}` masks `{cli::col_blue(package)}::{both}()`."
  )
  c(set_names(bullets, "x"), more)
}

uses_testthat <- function(path = ".") {
  paths <- c(
    package_file("inst", "tests", path = path),
    package_file("tests", "testthat", path = path)
  )

  any(dir.exists(paths)) && requireNamespace("testthat", quietly = TRUE)
}

find_test_dir <- function(path) {
  testthat <- package_file("tests", "testthat", path = path)
  if (dir.exists(testthat)) {
    return(testthat)
  }

  inst <- package_file("inst", "tests", path = path)
  if (dir.exists(inst)) {
    return(inst)
  }

  cli::cli_abort("No testthat directories found in {.path {path}}.")
}

is_foreign_method <- function(x, package) {
  env <- environment(x)
  !is_namespace(env) || !is_string(ns_env_name(env), package)
}

#' @rdname load_all
#' @param pkg If supplied, `is_loading()` only returns `TRUE` if the
#'   package being loaded is `pkg`.
#' @export
is_loading <- function(pkg = NULL) {
  var <- Sys.getenv("DEVTOOLS_LOAD")
  if (is_null(pkg)) {
    nzchar(var)
  } else {
    is_string(var, pkg)
  }
}

# Ensure that calls to `::` resolve to the original detached namespace. It is
# uncommon to refer to functions in your own package with `::` but it sometimes
# is necessary, e.g. in standalone files.
#
# To enable this, assign `::` in your namespace, e.g.
#
# ```
# on_load(`::` <- base::`::`)
# ```
patch_colon <- function(package) {
  ns <- asNamespace(package)

  if (!env_has(ns, "::")) {
    return()
  }

  rlang::env_binding_unlock(ns, "::")
  on.exit(rlang::env_binding_lock(ns, "::"))

  ns[["::"]] <- function(lhs, rhs) {
    lhs <- as.character(substitute(lhs))
    rhs <- as.character(substitute(rhs))

    if (identical(lhs, package)) {
      if (!exists(rhs, envir = ns, inherits = FALSE)) {
        stop(sprintf("Can't find `%s` in %s.", rhs, lhs))
      }
      ns[[rhs]]
    } else {
      eval(bquote(base::`::`(.(lhs), .(rhs))), baseenv())
    }
  }
}
