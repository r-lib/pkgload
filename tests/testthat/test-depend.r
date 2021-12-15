local_load_all_quiet()

test_that("Warn about mismatched version", {
  # Should give a warning about grid version
  expect_error(
    load_all("testImportVersion"),
    class = "rlib_error_package_not_found"
  )
  unload("testImportVersion")
})

test_that("Error on missing dependencies", {
  expect_error(
    load_all("testImportMissing"),
    class = "rlib_error_package_not_found"
  )

  # Loading process will be partially done; unload it
  unload("testImportMissing")
})

test_that("Packages in depends are required", {
  load_all("testDependMissing")
  expect_true("package:bitops" %in% search())
  unload("testDependMissing")
  detach("package:bitops", unload = TRUE)
})

test_that("Parse dependencies", {
  deps <- parse_deps("\nhttr (< 2.1),\nRCurl (>= 3),\nutils (== 2.12.1),\ntools,\nR (>= 2.10),\nmemoise")
  expect_equal(nrow(deps), 5)
  expect_false("R" %in% deps$name)

  expect_equal(deps$compare, c("<", ">=", "==", NA, NA))
  expect_equal(deps$version, c("2.1", "3", "2.12.1", NA, NA))


  # Invalid version specifications
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (3.0)"))
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl ( 3.0)"))
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (==3.0)"))
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (==3.0 )"))
  expect_error(parse_deps("\nhttr (< 2.1),\nRCurl ( ==3.0)"))

  # This should be OK (no error)
  deps <- parse_deps("\nhttr (< 2.1),\nRCurl (==  3.0.1)")
  expect_equal(deps$compare, c("<", "=="))
  expect_equal(deps$version, c("2.1", "3.0.1"))
})

test_that("Declared dependencies are added to .Depends object", {
  load_all("testDependsExists")
  expect_equal(get(".Depends", "package:testDependsExists", inherits = FALSE),
                "httr")
  unload("testDependsExists")
})
