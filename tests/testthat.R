library(testthat)
library(pkgload)

# Needed so that install.packages works correctly
Sys.setenv("R_TESTS" = "")

test_check("pkgload")
