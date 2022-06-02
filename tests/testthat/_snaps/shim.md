# shimmed system.file respects mustWork

    Code
      (expect_error(find_missing(TRUE), "Can't find package file."))
    Output
      <error/rlang_error>
      Error:
      ! Can't find package file.

# system.file() fails if path starts with `inst` (#104)

    Code
      (expect_error(shim_system.file("inst/WORDLIST", package = "pkgload", mustWork = TRUE))
      )
    Output
      <error/rlang_error>
      Error in `shim_system.file()`:
      ! Paths can't start with `inst`
      i Files in `inst` are installed at top-level.
    Code
      (expect_error(shim_system.file("inst", "WORDLIST", package = "pkgload",
        mustWork = TRUE)))
    Output
      <error/rlang_error>
      Error in `shim_system.file()`:
      ! Paths can't start with `inst`
      i Files in `inst` are installed at top-level.

