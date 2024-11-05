.onLoad <- function(libname, pkgname) {
  run_on_load()
  ns <- ns_env(pkgname)

  # Capture TEMPDIR for use in subprocesses
  if (identical(Sys.getenv("PKGLOAD_PARENT_TEMPDIR"), "")) {
    Sys.setenv("PKGLOAD_PARENT_TEMPDIR" = tempdir())
  }

  # Force reload global shims if developing pkgload itself
  if (is_loading()) {
    insert_global_shims(force = TRUE)
  }

  nms <- fn_env(onload_assign)$names
  funs <- fn_env(onload_assign)$funs

  for (i in seq_along(nms)) {
    env_poke(ns, nms[[i]], eval(funs[[i]], ns))
  }
}

# These functions are used in load_all() so need to exist in the
# devtools namespace so the desc namespace is not prematurely loaded
# by `::` during a load_all() call.
#
# They are lazily assigned to avoid racing issues while installing in
# parallel (see #89), and forced via `force_load_all_deps()` before
# unregistering namespaces.
on_load({
  desc_desc %<~% desc::desc
  desc_desc_get_field %<~% desc::desc_get_field
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
    desc_desc,
    desc_desc_get_field,
    desc_desc_get_version,
    rprojroot_find_package_root_file,
    testthat_source_test_helpers
  )
}

# R CMD check NOTE
unused <- function() {
  desc::desc
  rprojroot::find_package_root_file
}
