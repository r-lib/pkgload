.onLoad <- function(libname, pkgname) {
  # These withr functions are used in load_all() so need to exist in the
  # devtools namespace so the withr namespace is not prematurely loaded by `::`
  # during a load_all() call
  env <- asNamespace(pkgname)
  assign("withr_with_dir", withr::with_dir, envir = env)
  assign("withr_with_collate", withr::with_collate, envir = env)
  assign("withr_with_envvar", withr::with_envvar, envir = env)
  assign("desc_desc", desc::desc, envir = env)
  assign("desc_desc_get", desc::desc_get, envir = env)
  assign("desc_desc_get_version", desc::desc_get_version, envir = env)

  nms <- environment(onload_assign)$names
  funs <- environment(onload_assign)$funs
  for (i in seq_along(nms)) {
    assign(nms[[i]], eval(funs[[i]], envir = env), envir = env)
  }

  invisible()
}
