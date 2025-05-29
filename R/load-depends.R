load_depends <- function(path = ".", quiet = NULL) {
  path <- pkg_path(path)
  quiet <- load_all_quiet(quiet, "load_depends")

  description <- pkg_desc(path)

  # Get data frame of dependency names and versions
  deps <- description$get_deps()
  depends <- deps[deps$type == "Depends" & deps$package != "R", ]
  if (length(depends) == 0) {
    return(invisible())
  }

  deps_check_installed(path, depends)

  # The `quietly` argument of `require()` does not affect attachment
  # of required packages
  if (quiet) {
    maybe_suppress <- suppressMessages
  } else {
    maybe_suppress <- identity
  }
  maybe_suppress(
    lapply(depends$package, require, character.only = TRUE)
  )

  invisible(depends)
}
