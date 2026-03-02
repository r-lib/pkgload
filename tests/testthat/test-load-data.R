local_load_all_quiet()

test_that("load_data can load .R files that use utils functions in a clean environment", {
  # We use a subprocess with R_DEFAULT_PACKAGES=NULL to simulate an
  # environment where 'utils' is not attached.
  
  # Ensure the test package and pkgload paths are absolute
  pkg_path <- normalizePath(test_path("testDataUtils"), mustWork = TRUE)
  pkgload_path <- normalizePath(package_file(), mustWork = TRUE)
  
  # We use callr::r() to run a subprocess with a controlled environment.
  expect_equal(
    callr::r(
      function(pkg_path, pkgload_path) {
        # Load the development version of pkgload, which has our fix
        pkgload::load_all(pkgload_path)
        # Load the test package using the fixed pkgload
        pkgload::load_all(pkg_path)
        testDataUtils::myData$a
      },
      args = list(pkg_path = pkg_path, pkgload_path = pkgload_path),
      env = c(R_DEFAULT_PACKAGES = "NULL")
    ),
    1L
  )
})
