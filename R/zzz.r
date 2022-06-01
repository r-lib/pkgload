.onLoad <- function(libname, pkgname) {
  run_on_load()
  ns <- ns_env(pkgname)

  nms <- fn_env(onload_assign)$names
  funs <- fn_env(onload_assign)$funs

  for (i in seq_along(nms)) {
    env_poke(ns, nms[[i]], eval(funs[[i]], ns))
  }
}

# These functions are used in load_all() so need to exist in the
# devtools namespace so the withr namespace is not prematurely loaded
# by `::` during a load_all() call.
#
# They are lazily assigned to avoid racing issues while installing in
# parallel (see #89), and forced via `force_load_all_deps()` before
# unregistering namespaces.
on_load({
  withr_with_dir %<~% withr::with_dir
  withr_with_collate %<~% withr::with_collate
  withr_with_envvar %<~% withr::with_envvar

  desc_desc %<~% desc::desc
  desc_desc_get %<~% desc::desc_get
  desc_desc_get_version %<~% desc::desc_get_version

  rprojroot_find_package_root_file %<~% rprojroot::find_package_root_file

  if (is_installed("testthat")) {
    testthat_source_test_helpers %<~% testthat::source_test_helpers
  } else {
    testthat_source_test_helpers %<~% function(...) TRUE
  }
})

force_load_all_deps <- function() {
  list(
    withr_with_dir,
    withr_with_collate,
    withr_with_envvar,
    desc_desc,
    desc_desc_get,
    desc_desc_get_version,
    rprojroot_find_package_root_file,
    testthat_source_test_helpers
  )
}
