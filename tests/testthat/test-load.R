local_load_all_quiet()

test_that("Package root and subdirectory is working directory when loading", {
  expect_message(load_all("testLoadDir"), "[|].*/testLoadDir[|]")
  expect_message(load_all(file.path("testLoadDir", "R")), "[|].*/testLoadDir[|]")
})

test_that("helpers are available after load_all", {
  load_all("testLoadHelpers")

  # object defined R/
  expect_equal(baz, 1)

  # object defined in a helper
  expect_equal(foo, 1)

  # object defined in helper, referencing lazy data object mtcars2
  expect_equal(head_mtcars, head(mtcars2))

  # object defined in helper using explicitly qualified package name
  expect_equal(helper_baz, baz)

  unload("testLoadHelpers")
})

test_that("`quiet` argument works (#213)", {
  expect_message(load_all(test_path("testLoadHelpers"), quiet = FALSE), "Loading")
  expect_no_message(load_all(test_path("testLoadHelpers"), quiet = TRUE))
})

test_that("warn_if_conflicts warns for conflicts and both objects are functions", {
  e1 <- new.env(parent = emptyenv())
  e2 <- new.env(parent = emptyenv())

  e1$foo <- function() "foo"
  e2$bar <- function() "bar"

  # no warning if no conflicts
  expect_warning(warn_if_conflicts("pkg", e1, e2), NA)

  e2$foo <- function() "foo2"

  # warning for a conflict
  expect_snapshot({
    (expect_warning(warn_if_conflicts("pkg", e1, e2)))
  })
})

test_that("warn_if_conflicts does not warn for conflicts when one of the objects is not a function", {
  e1 <- new.env(parent = emptyenv())
  e2 <- new.env(parent = emptyenv())

  e1$foo <- function() "foo"
  e2$foo <- "foo"

  expect_warning(warn_if_conflicts("pkg", e1, e2), NA)
})

test_that("unloading or reloading forces bindings", {
  forced <- FALSE

  withCallingHandlers(
    forced = function(...) forced <<- TRUE, {
      # Allow running test interactively
      on.exit(unload("testLoadLazy"))

      # On older R versions, `env_coalesce()` forces bindings
      attach <- getRversion() >= "4.0.0"

      load_all("testLoadLazy", attach = attach)
      expect_false(forced)

      load_all("testLoadLazy")
      expect_true(forced)
    }
  )
})

test_that("reloading a package unloads deleted S3 methods", {
  x <- structure(list(), class = "pkgload_foobar")

  load_all("testS3removed")
  expect_equal(as.character(x), "registered")

  # Hold a reference to the generic in the currently loaded namespace
  stale_generic <- testS3removed::my_generic

  load_all("testS3removed2")
  expect_equal(as.character(x), character())

  # Still works because we don't unregister methods for the package
  # being unloaded (r-lib/vctrs#1341)
  expect_equal(stale_generic(x), "registered")
})

test_that("reloading a package reloads foreign methods (#163)", {
  x <- structure(list(), class = "foreign_foobar")

  load_all("testS3removed")

  registerS3method(
    "my_generic",
    "foreign_foobar",
    function(...) "OK",
    envir = ns_env("testS3removed")
  )
  expect_equal(my_generic(x), "OK")

  load_all("testS3removed")
  expect_equal(my_generic(x), "OK")
})

test_that("reloading a package reloads own methods", {
  x <- structure(list(), class = "pkgload_foobar")

  load_all("testS3removed")

  ns <- ns_env("testS3removed")
  method <- function(...) "Not OK"
  environment(method) <- ns

  registerS3method(
    "my_generic",
    "pkgload_foobar",
    method,
    envir = ns
  )

  # `registerS3method()` doesn't seem to overwrite methods on older
  # versions of R < 3.5.0.
  if (is_installed("base", "3.5.0")) {
    expect_equal(my_generic(x), "Not OK")
  }

  load_all("testS3removed")
  expect_equal(my_generic(x), "registered")
})

test_that("load_all() errors when no DESCRIPTION found", {
  withr::with_tempdir({
    (expect_error(load_all(), class = "pkgload_no_desc"))
  })
})

test_that("can load without attaching", {
  load_all("testLoadAttach", attach = FALSE)
  expect_false(is_attached("testLoadAttach"))

  load_all("testLoadAttach", attach = TRUE)
  expect_true(is_attached("testLoadAttach"))
})

test_that("internal functions exported to the search path are not imported in downstream packages", {
  # This package has an internal function called `internal`
  load_all(test_path("testLoadImportUpstream"))

  # This package exports a function called `internal`
  load_all(test_path("testLoadImportUpstreamAlt"))

  # This package imports both packages above
  expect_no_warning(
    load_all(test_path("testLoadImportDownstream"))
  )
})
