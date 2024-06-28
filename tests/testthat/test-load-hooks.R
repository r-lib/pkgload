local_load_all_quiet()

test_that("hooks called in correct order", {
  record_use <- function(hook) {
    function(...) {
      h <- globalenv()$hooks
      h$events <- c(h$events, hook)
    }
  }
  reset_events <- function() {
    assign("hooks", new.env(parent = emptyenv()), envir = globalenv())
    h <- globalenv()$hooks
    h$events <- character()
  }


  setHook(packageEvent("testHooks", "attach"), record_use("user_attach"))
  setHook(packageEvent("testHooks", "detach"), record_use("user_detach"))
  setHook(packageEvent("testHooks", "onLoad"),   record_use("user_load"))
  setHook(packageEvent("testHooks", "onUnload"), record_use("user_unload"))

  reset_events()
  load_all("testHooks")
  expect_equal(globalenv()$hooks$events,
    c("pkg_load", "user_load", "pkg_attach", "user_attach")
  )

  reset_events()
  unload("testHooks")
  expect_equal(globalenv()$hooks$events,
    c("user_detach", "pkg_detach", "user_unload", "pkg_unload")
  )

  rm(list = "hooks", envir = globalenv())
  setHook(packageEvent("testHooks", "attach"), NULL, "replace")
  setHook(packageEvent("testHooks", "detach"), NULL, "replace")
  setHook(packageEvent("testHooks", "onLoad"),   NULL, "replace")
  setHook(packageEvent("testHooks", "onUnload"), NULL, "replace")

})

test_that("onLoad and onAttach", {
  ran <- FALSE
  with_options(
    "pkgload:::testLoadHooks::.onLoad" = function() {
      expect_true(is_loading())
      expect_true(is_loading("testLoadHooks"))
      expect_false(is_loading("foobar"))
      ran <<- TRUE
    },
    load_all("testLoadHooks")
  )

  expect_true(ran)

  nsenv <- ns_env("testLoadHooks")
  pkgenv <- pkg_env("testLoadHooks")

  the <- nsenv$the
  expect_true(is_reference(the, pkgenv$the))

  # normalizePath is needed so that capitalization differences on
  # case-insensitive platforms won't cause errors.
  expect_equal(normalizePath(the$onload_lib), normalizePath(getwd()))
  expect_equal(normalizePath(the$onattach_lib), normalizePath(getwd()))

  expect_false(nsenv$ns_locked)
  expect_true(nsenv$pkg_locked)

  # a: modified by onLoad in namespace env
  # b: modified by onAttach in namespace env
  # c: modified by onAttach in package env (no longer the case because
  # internal bindings are no longer populated at the time the `onAttach`
  # hook is run)
  expect_equal(the$a, 2)
  expect_equal(the$b, 2)
  expect_equal(the$c, 1)

  # Shouldn't form new environments
  expect_identical(nsenv, ns_env("testLoadHooks"))
  expect_identical(pkgenv, pkg_env("testLoadHooks"))

  expect_equal(the$a, 2)
  expect_equal(the$b, 2)
  expect_equal(the$c, 1)

  # ===================================================================
  # When loading again there should be new package and namespace
  # environments, and the values should be the same as the first
  # load_all.
  load_all("testLoadHooks")
  nsenv2 <- ns_env("testLoadHooks")
  pkgenv2 <- pkg_env("testLoadHooks")

  the2 <- nsenv2$the

  # Should form new environments
  expect_false(identical(nsenv, nsenv2))
  expect_false(identical(pkgenv, pkgenv2))

  # Values should be same as first time
  expect_equal(the2$a, 2)
  expect_equal(the2$b, 2)
  expect_equal(the2$c, 1)

  unload("testLoadHooks")

  # ===================================================================
  # Unloading and reloading should create new environments and same
  # values as first time
  load_all("testLoadHooks")
  nsenv3 <- ns_env("testLoadHooks")
  pkgenv3 <- pkg_env("testLoadHooks")
  the3 <- nsenv3$the

  # Should form new environments
  expect_false(identical(nsenv, nsenv3))
  expect_false(identical(pkgenv, pkgenv3))

  # Values should be same as first time
  expect_equal(the3$a, 2)
  expect_equal(the3$b, 2)
  expect_equal(the3$c, 1)

  unload("testLoadHooks")
})

test_that("onUnload", {
  load_all("testLoadHooks")

  # The onUnload function in testLoadHooks increments this variable
  .GlobalEnv$.__testLoadHooks__ <- 1
  unload("testLoadHooks")
  expect_equal(.GlobalEnv$.__testLoadHooks__, 2)

  # Clean up
  rm(".__testLoadHooks__", envir = .GlobalEnv)
})

test_that("user onLoad hooks are properly run", {
  load_all("testUserLoadHook")

  expect_condition(
    load_all("testUserLoadHookUpstream"),
    class = "hook_was_run"
  )

  unload("testUserLoadHook")
  unload("testUserLoadHookUpstream")
})

test_that("user onLoad hooks are properly run", {
  load_all("testUserLoadHookError")

  expect_snapshot({
    (expect_warning(
      expect_condition(
        load_all("testUserLoadHookUpstream"),
        class = "hook_was_run_error"
      )
    ))
  })

  unload("testUserLoadHookError")
  unload("testUserLoadHookUpstream")
})
