#' Load data.
#'
#' Loads all `.RData` files in the data subdirectory.
#'
#' @inheritParams load_all
#' @keywords programming
#' @export
load_data <- function(path = ".") {
  # Note that this simulates normal R package loading by placing the data sets
  # in the .__NAMESPACE__.$lazydata environment, but unlike with proper lazy
  # loading via lazyLoad(), we'll need to manually copy these objects over to
  # the package environment later.

  path <- pkg_path(path)
  nsenv <- ns_env(pkg_name(path))
  lazydata_env <- nsenv$.__NAMESPACE__.$lazydata
  objs <- character()

  sysdata <- package_file("R", "sysdata.rda", path = path)
  if (file.exists(sysdata)) {
    objs <- c(objs, load(sysdata, envir = nsenv))
  }

  path_data <- package_file("data", path = path)
  if (file.exists(path_data)) {
    paths <- dir(path_data, "\\.[rR][dD]a(ta)?$", full.names = TRUE)
    paths <- changed_files(paths)
    objs <- c(objs, unlist(lapply(paths, load, envir = lazydata_env)))

    paths <- dir(path_data, "\\.[rR]$", full.names = TRUE)
    paths <- changed_files(paths)
    if (length(paths) > 0) {
      # The utils package is not attached by default in some R environments
      # (e.g. R_DEFAULT_PACKAGES=NULL). R's native utils::data() function
      # explicitly attaches 'utils' before sourcing .R files.
      library("utils")

      # We source the .R files into lazydata_env, but we want them to be
      # able to find functions in the default packages like utils.
      # By default lazydata_env inherits from base, bypassing the search path.
      # We temporarily change its parent to .GlobalEnv so it can see the search path.
      old_parent <- parent.env(lazydata_env)
      parent.env(lazydata_env) <- .GlobalEnv
      on.exit(parent.env(lazydata_env) <- old_parent, add = TRUE)

      objs <- c(
        objs,
        unlist(lapply(
          paths,
          sys.source,
          envir = lazydata_env,
          chdir = TRUE,
          keep.source = TRUE
        ))
      )
    }
  }

  invisible(objs)
}
