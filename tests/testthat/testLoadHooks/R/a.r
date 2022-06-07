ns_locked <- TRUE
pkg_locked <- TRUE

the <- new.env()
the$a <- 1
the$b <- 1
the$c <- 1

the$onload_lib <- ""
the$onattach_lib <- ""

.onLoad <- function(lib, pkg) {
  hook <- getOption("pkgload:::testLoadHooks::.onLoad")
  if (is.function(hook)) {
    hook()
  }

  # Namespace is not sealed at this point
  ns_locked <<- FALSE

  the$onload_lib <- lib
  the$a <- the$a + 1
}

.onAttach <- function(lib, pkg) {
  the$onattach_lib <- lib

  # Attempt to modify b in namespace. This throws an error because the
  # namespace is sealed at this point.
  expect_error(pkg_locked <<- FALSE)

  the$b <- the$b + 1

  # FIXME: The package env should not be populated with internal
  # helpers when the hook is run
  env <- as.environment("package:testLoadHooks")
  if (!is.null(env$the)) {
    the$c <- the$c + 1
  }
}

.onUnload <- function(libpath) {
  # Increment this variable if it exists in the global env
  if (exists(".__testLoadHooks__", .GlobalEnv)) {
    .GlobalEnv$.__testLoadHooks__ <- .GlobalEnv$.__testLoadHooks__ + 1
  }
}
