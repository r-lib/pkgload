#' Run user and package hooks.
#'
#'
#' @inheritParams ns_env
#' @param hook hook name: one of "load", "unload", "attach", or "detach"
#' @keywords internal
run_pkg_hook <- function(package, hook) {
  trans <- c(
    "load" = ".onLoad",
    "unload" = ".onUnload",
    "attach" = ".onAttach",
    "detach" = ".onDetach"
  )
  hook <- match.arg(hook, names(trans))
  f_name <- trans[hook]

  metadata <- dev_meta(package)
  if (isTRUE(metadata[[f_name]])) {
    return(FALSE)
  }

  # Run hook function if defined, and not already run
  nsenv <- ns_env(package)
  ns_path <- ns_path(package)

  if (!exists(f_name, nsenv, inherits = FALSE)) {
    return(FALSE)
  }

  if (hook %in% c("load", "attach")) {
    nsenv[[f_name]](dirname(ns_path), package)
  } else {
    # `.onUnload()` and `.onDetach()` take the full path to the package
    nsenv[[f_name]](ns_path)
  }
  metadata[[f_name]] <- TRUE

  TRUE
}

#' @rdname run_pkg_hook
run_user_hook <- function(package, hook) {
  nsenv <- ns_env(package)

  trans <- c(
    "load" = "onLoad",
    "unload" = "onUnload",
    "attach" = "attach",
    "detach" = "detach"
  )
  hook <- match.arg(hook, names(trans))
  hook_name <- trans[hook]

  ns_path <- ns_path(package)
  lib_path <- dirname(ns_path)

  metadata <- dev_meta(package)
  if (isTRUE(metadata[[hook_name]])) {
    return(FALSE)
  }

  hooks <- getHook(packageEvent(package, hook_name))
  if (length(hooks) == 0) {
    return(FALSE)
  }

  for (fun in rev(hooks)) {
    try_fetch(
      if (hook %in% c("load", "attach")) {
        fun(package, lib_path)
      } else if (hook == "unload") {
        # `unloadNamespace()` passes the full path to the package
        fun(package, ns_path)
      } else {
        fun(package)
      },
      error = function(cnd) {
        msg <- sprintf(
          "Problem while running user `%s` hook for package %s.",
          hook_name,
          package
        )

        name <- env_name(topenv(fn_env(fun)))
        if (nzchar(name)) {
          msg <- c(msg, i = sprintf("The hook inherits from `%s`.", name))
        }

        cli::cli_warn(msg, parent = cnd)
      }
    )
  }
  metadata[[hook_name]] <- TRUE
  invisible(TRUE)
}

# Run the unload hooks of a package that `load_all()` is about to
# reload. Reloading doesn't unload the namespace or its DLL (to allow
# safe usage of dangling references), so `unloadNamespace()` never gets
# a chance to run `.onUnload()` and the user `onUnload` hooks (#253).
# Errors are demoted to warnings so that a failing hook can't prevent
# reloading.
run_unload_hooks <- function(package) {
  run_user_hook(package, "unload")

  try_fetch(
    run_pkg_hook(package, "unload"),
    error = function(cnd) {
      cli::cli_warn(
        "Problem while running `.onUnload()` for package {.pkg {package}}.",
        parent = cnd
      )
    }
  )
}
