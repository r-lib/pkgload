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
  expect_error(utils::`?`(foofoo(123)))
  expect_error(shim_question(foofoo(123)))
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
    print(h1, type = 'text'))
})

test_that("dev_help works with package and function help with the same name", {
  load_all(test_path('testHelp'))
  on.exit(unload(test_path('testHelp')))

  h1 <- dev_help("testHelp")
  expect_identical(shim_question(testHelp::testHelp), h1)
})
