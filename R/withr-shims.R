defer <- function (expr, env = caller_env(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = env)
}

local_envvar <- function(..., .frame = parent.frame()) {
  old <- set_envvar(list(...))
  defer(set_envvar(old), .frame)

  invisible()
}

local_collate <- function(locale, frame = parent.frame()) {
  old <- Sys.getlocale("LC_COLLATE")
  defer(Sys.setlocale("LC_COLLATE", old), frame)
  Sys.setlocale("LC_COLLATE", locale)

  # From https://github.com/r-lib/withr/blob/v3.0.0/R/locale.R#L51-L55:
  # R supports setting LC_COLLATE to C via envvar. When that is the
  # case, it takes precedence over the currently set locale. We need
  # to set both the envvar and the locale for collate to fully take
  # effect.
  local_envvar(LC_COLLATE = locale, .frame = frame)

  invisible()
}

local_dir <- function(path, frame = parent.frame()) {
  old <- setwd(path)
  defer(setwd(old), frame)

  invisible()
}

# adapted from withr:::set_envvar
set_envvar <- function(envs) {
  if (length(envs) == 0) {
    return()
  }

  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)

  if (any(set)) do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set)) Sys.unsetenv(names(envs)[!set])

  invisible(old)
}
