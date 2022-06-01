local_load_all_quiet <- function(frame = caller_env()) {
  local_options("testthat:::load_all_quiet" = TRUE, .frame = frame)
}

expect_no_warning <- function(object) {
  expect_warning({{ object }}, NA)
}
