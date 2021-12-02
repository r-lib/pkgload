local_load_all_quiet <- function(frame = caller_env()) {
  local_options("testthat:::load_all_quiet" = TRUE, .frame = frame)
}
