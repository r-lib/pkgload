test_that("load_code() can be called directly, without an existing namespace (#335)", {
  defer(unload("testLoadCode"))

  load_code("testLoadCode")
  expect_equal(ns_env("testLoadCode")$foo(), 1)
})
