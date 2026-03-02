local_load_all_quiet()

test_that("load_data can load .R files that use utils functions", {
  # Simulate an environment where utils might not be attached
  # Though in tests it usually is.
  # We can't easily set R_DEFAULT_PACKAGES=NULL here, but we can 
  # check that it works in the current environment.
  
  load_all("testDataUtils")
  expect_equal(myData$a, 1)
  unload("testDataUtils")
})
