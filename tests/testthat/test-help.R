local_load_all_quiet()

test_that("shim_help behaves the same as utils::help for non-devtools-loaded packages", {
  # stats wasn't loaded with devtools. There are many combinations of calling
  # with quotes and without; make sure they're the same both ways. Need to index
  # in using [1] to drop attributes for which there are unimportant differences.
  expect_identical(shim_help(lm)[1],            utils::help(lm)[1])
  expect_identical(shim_help(lm, stats)[1],     utils::help(lm, stats)[1])
  expect_identical(shim_help(lm, 'stats')[1],   utils::help(lm, 'stats')[1])
  expect_identical(shim_help('lm')[1],          utils::help('lm')[1])
  expect_identical(shim_help('lm', stats)[1],   utils::help('lm', stats)[1])
  expect_identical(shim_help('lm', 'stats')[1], utils::help('lm', 'stats')[1])
  expect_identical(shim_help(, "stats")[1],     utils::help(, "stats")[1])

  # Works for :: and ::: as well (#72)
  expect_identical(shim_help("::")[1],          utils::help("::")[1])
  expect_identical(shim_help(":::")[1],         utils::help(":::")[1])
})

test_that("shim_help behaves the same as utils::help for nonexistent objects", {
  expect_equal(length(shim_help(foofoo)), 0)
  expect_equal(length(shim_help("foofoo")), 0)
})


test_that("shim_question behaves the same as utils::? for non-devtools-loaded packages", {
  expect_identical(shim_question(lm)[1], utils::`?`(lm)[1])
  expect_identical(shim_question(stats::lm)[1], utils::`?`(stats::lm)[1])
  expect_identical(shim_question(lm(123))[1], utils::`?`(lm(123))[1])
  expect_identical(shim_question(`lm`)[1], utils::`?`(`lm`)[1])
  expect_identical(shim_question('lm')[1], utils::`?`('lm')[1])
  expect_identical(shim_question(base::min)[1], utils::`?`(base::min)[1])
})

test_that("shim_question behaves like util::? for searches", {
  expect_identical(shim_question(?lm), utils::`?`(?lm))
})

test_that("shim_question behaves the same as utils::? for nonexistent objects", {
  expect_equal(length(shim_question(foofoo)), 0)
  expect_equal(length(shim_question(`foofoo`)), 0)
  expect_equal(length(shim_question("foofoo")), 0)

  # If given a function call with nonexistent function, error
  expect_snapshot({
    (expect_error(utils::`?`(foofoo(123))))
    (expect_error(shim_question(foofoo(123))))
  })
})

test_that("show_help and shim_question files for devtools-loaded packages", {
  load_all(test_path('testHelp'))
  on.exit(unload(test_path('testHelp')))

  h1 <- shim_help("foofoo")
  expect_s3_class(h1, "dev_topic")
  expect_equal(h1$topic, "foofoo")
  expect_equal(h1$pkg, "testHelp")

  expect_identical(shim_help(foofoo), h1)
  expect_identical(shim_help(foofoo, "testHelp"), h1)
  expect_identical(shim_question(testHelp::foofoo), h1)

  pager_fun <- function(files, header, title, delete.file) {
    expect_equal(title, "testHelp:foofoo.Rd")
  }

  withr::with_options(
    c(pager = pager_fun),
    suppressMessages(
      print(h1, type = 'text')
    ))
})

test_that("dev_help works with package and function help with the same name", {
  load_all(test_path('testHelp'))
  on.exit(unload(test_path('testHelp')))

  h1 <- dev_help("testHelp")
  expect_identical(shim_question(testHelp::testHelp), h1)
})

test_that("dev_help gives clear error if no packages loaded", {
  mockr::local_mock(dev_packages = function() character())
  expect_snapshot(dev_help("foo"), error = TRUE)
})

test_that("unknown macros don't trigger warnings (#119)", {
  load_all(test_path("testUnknownMacro"))

  expect_no_warning(
    out <- dev_help("testUnknownMacro")
  )

  # Because we're testing internal behaviour in `tools`
  skip_on_cran()

  # Because on RStudio the print method uses a different method
  skip_if(is_rstudio())

  # We should still be displaying a warning when rendering the documentation
  local_options(pager = function(...) "")
  suppress_output(
    expect_warning(print(out), "unknown macro")
  )
})

test_that("complex expressions are checked", {
  expect_snapshot({
    (expect_error(shim_help({ foo; bar }), "must be a name"))
  })
})

test_that("can use macros in other packages (#120)", {
  expect_true(has_rd_macros(test_path("testMacroDownstream/")))

  skip_if_not_installed("mathjaxr")

  load_all(test_path("testMacroDownstream"))

  topic <- dev_help("macro_downstream")
  text_lines <- topic_lines(topic, "text")
  html_lines <- topic_lines(topic, "html")

  expect_true(any(grepl("foreign macro success", text_lines)))
  expect_true(any(grepl("foreign macro.*success", html_lines)))
})

test_that("httpdPort() is available", {
  skip_on_cran()
  # We're using this unexported function to open help pages in RStudio
  expect_true(is.function(httpdPort))
})
