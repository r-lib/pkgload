local_load_all_quiet <- function(value = TRUE, frame = caller_env()) {
  local_options("testthat:::load_all_quiet_default" = value, .frame = frame)
}

expect_no_warning <- function(object) {
  expect_warning({{ object }}, NA)
}
expect_no_message <- function(object) {
  expect_message({{ object }}, NA)
}

suppress_output <- function(expr) {
  capture.output(
    capture.output(
      expr,
      type = "message"
    ),
    type = "output"
  )
}

# Used to skip tests that will fail when locale is set to C, for
# instance `with_language()` tests. These tests should only be run
# when using a locale like `en_US`.
skip_if_c_locale <- function() {
  lc_all <- Sys.getenv("LC_ALL", "")
  skip_if(lc_all %in% c("C", "C.UTF-8"))

  if (lc_all == "") {
    lang <- Sys.getenv("LANG", "")
    skip_if(lang %in% c("C", "C.UTF-8"))
  }
}
