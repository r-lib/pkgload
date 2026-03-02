test_that("load_data can load .R files that use utils functions in a clean environment", {
  pkg_path <- normalizePath(test_path("testDataUtils"), mustWork = TRUE)
  
  expect_equal(
    callr::r(
      function(pkg_path) {
        pkgload::load_all(pkg_path)
        testDataUtils::myData$a
      },
      args = list(pkg_path = pkg_path),
      env = c(R_DEFAULT_PACKAGES = "NULL")
    ),
    1L
  )
})
