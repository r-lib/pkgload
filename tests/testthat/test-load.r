context("Loading")

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

  # object definde in helper, referencing lazy data object mtcars2
  expect_equal(head_mtcars, head(mtcars2))

  # object defined in helper using explicitly qualified package name
  expect_equal(helper_baz, baz)

  unload("testLoadHelpers")
})

test_that("warn_if_conflicts works", {
  # no warning if no conflicts
  expect_warning(warn_if_conflicts("pkg", c("foo"), c("bar")), NA)

  # warning for a conflict
  withr::with_options(c(crayon.enabled = FALSE),
    expect_warning(
      warn_if_conflicts("pkg", c("foo"), c("foo", "bar")),
      "foo().*masks.*pkg::foo()"
    )
  )
})

test_that("loading multiple times doesn't force bindings", {
  forced <- FALSE

  withCallingHandlers(
    forced = function(...) forced <<- TRUE, {
      load_all("testLoadLazy")
      expect_false(forced)

      load_all("testLoadLazy")
      expect_false(forced)
    }
  )
})

test_that("reloading a package unloads deleted S3 methods", {
  x <- structure(list(), class = "pkgload_foobar")

  load_all("testS3removed")
  expect_equal(as.character(x), "registered")

  load_all("testS3removed2")
  expect_equal(as.character(x), character())
})
