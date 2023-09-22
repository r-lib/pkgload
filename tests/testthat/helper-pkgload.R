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

# Need to also specify `LC_ALL` because `LANGUAGE` is ignored when
# `LANG` is set (here via `LC_ALL`) to `C` or `C.UTF-8`
with_lang <- function(lc, language, expr) {
  withr::local_envvar(c(LC_ALL = lc))
  withr::local_language(language)
  expr
}
