local_load_all_quiet()

test_that("default run_example ignores donttest and dontrun ", {
  env <- run_example(test_path("test-examples.Rd"), quiet = TRUE)
  expect_equal(env$a, 1)
})

test_that("run donttest when requested", {
  env <- run_example(test_path("test-examples.Rd"), run_donttest = TRUE, quiet = TRUE)
  expect_equal(env$a, 2)
})

test_that("run dontrun when requested", {
  env <- run_example(test_path("test-examples.Rd"), run_dontrun = TRUE, quiet = TRUE)
  expect_equal(env$a, 3)
})

test_that("can run example package", {
  load_all(test_path("testHelp"))
  on.exit(unload(test_path("testHelp")))

  env <- dev_example("foofoo", quiet = TRUE)
  expect_equal(env$a, 101)
})

test_that("can use system macros", {
  load_all(test_path("testHelp"))
  on.exit(unload(test_path("testHelp")))

  expect_silent(
    run_example(
      test_path("testHelp", "man", "testSysMacro.Rd"),
      quiet = TRUE
    )
  )
})

test_that("can use extra Rd macros", {
  macros <- load_rd_macros("testHelp")
  expect_silent(
    run_example(
      test_path("testHelp", "man", "testCustomMacro.Rd"),
      quiet = TRUE, macros = macros
    )
  )
})
