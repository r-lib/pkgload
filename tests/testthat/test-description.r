local_load_all_quiet()

test_that("Parse DESCRIPTION file", {
  pkg <- pkg_desc("testNamespace")

  expect_identical(package_version("0.1"), pkg$get_version())
  expect_identical("testNamespace", pkg_name("testNamespace"))
})
