the <- new.env()

# Can't fully reset onLoad hook so only run once in case of repeated
# `test()` calls
the$ran_already <- FALSE

.onLoad <- function(...) {
  setHook(
    packageEvent("testUserLoadHookUpstream", "onLoad"),
    function(...) {
      if (the$ran_already) {
        return()
      }
      the$ran_already <- TRUE

      rlang::signal("", "hook_was_run_error")
      stop("The message.")
    }
  )
}
