load_depends <- function(path = ".", quiet = FALSE) {
  path <- pkg_path(path)

  description <- pkg_desc(path)


  # Get data frame of dependency names and versions
  deps <- description$get_deps()
  depends <- deps[deps$type == "Depends" & deps$package != "R", ]
  if (length(depends) == 0) {
    return(invisible())
  }

  res <- mapply(check_dep_version, depends$package, depends$version)
  abort_for_missing_packages(res, depends$package)

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
