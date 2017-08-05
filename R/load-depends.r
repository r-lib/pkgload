load_depends <- function(path = ".") {
  path <- pkg_path(path)

  description <- pkg_desc(path)


  # Get data frame of dependency names and versions
  deps <- description$get_deps()
  depends <- deps[deps$type == "Depends" & deps$package != "R", ]
  if (length(depends) == 0) return(invisible())

  mapply(check_dep_version, depends$package, depends$version)
  lapply(depends$package, require, character.only = TRUE)

  invisible(depends)
}
