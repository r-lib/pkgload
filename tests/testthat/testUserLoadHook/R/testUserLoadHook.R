.onLoad <- function(...) {
  setHook(
    packageEvent("testUserLoadHookUpstream", "onLoad"),
    function(...) {
      # The package exports are populated when user onLoad hooks are run
      stopifnot(is.null(testUserLoadHookUpstream::foo()))

      rlang::signal("", "hook_was_run")
    }
  )
}
