load_depends <- function(path = ".") {
  path <- pkg_path(path)

  description <- pkg_desc(path)


  # Get data frame of dependency names and versions
  deps <- description$get_deps()
  depends <- deps[deps$type == "Depends" & deps$package != "R", ]
  if (length(depends) == 0) return(invisible())

  res <- mapply(check_dep_version, depends$package, depends$version)
  abort_for_missing_packages(res, imports$package)

  lapply(depends$package, require, character.only = TRUE)

  invisible(depends)
}
