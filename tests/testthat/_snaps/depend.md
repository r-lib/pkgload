# Warn about mismatched version

    Code
      (expect_error(load_all("testImportVersion"), class = "rlib_error_package_not_found")
      )
    Output
      <error/rlib_error_package_not_found>
      Error in `load_imports()`:
      ! The package "grid" (>= 100.0) is required.

# Error on missing dependencies

    Code
      (expect_error(load_all("testImportMissing"), class = "rlib_error_package_not_found")
      )
    Output
      <error/rlib_error_package_not_found>
      Error in `load_imports()`:
      ! The package "missingpackage" is required.

# Parse dependencies

    Code
      (expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (3.0)")))
    Output
      <error/rlang_error>
      Error in `parse_deps()`:
      ! Invalid comparison operator in dependency: RCurl (3.0).
    Code
      (expect_error(parse_deps("\nhttr (< 2.1),\nRCurl ( 3.0)")))
    Output
      <error/rlang_error>
      Error in `parse_deps()`:
      ! Invalid comparison operator in dependency: RCurl ( 3.0).
    Code
      (expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (==3.0)")))
    Output
      <error/rlang_error>
      Error in `parse_deps()`:
      ! Invalid comparison operator in dependency: RCurl (==3.0).
    Code
      (expect_error(parse_deps("\nhttr (< 2.1),\nRCurl (==3.0 )")))
    Output
      <error/rlang_error>
      Error in `parse_deps()`:
      ! Invalid comparison operator in dependency: ==3.0.
    Code
      (expect_error(parse_deps("\nhttr (< 2.1),\nRCurl ( ==3.0)")))
    Output
      <error/rlang_error>
      Error in `parse_deps()`:
      ! Invalid comparison operator in dependency: RCurl ( ==3.0).

