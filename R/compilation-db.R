# Should these tools move to pkgbuild?

use_compilation_db <- function() {
  check_installed("usethis")

  # Sneaky import won't be necessary once in usethis
  proj_desc_field_update <- env_get(ns_env("usethis"), "proj_desc_field_update")
  proj_desc_field_update("Config/build/compilation-database", "true")

  files <- c("compile_commands.json", ".cache")
  usethis::use_git_ignore(files)
  usethis::use_build_ignore(files)
}

generate_db <- function(path = ".") {
  check_installed("jsonlite")

  path <- pkg_path(path)
  package <- pkg_name(path)
  desc <- pkg_desc(path)

  src_path <- fs::path(path, "src")

  if (!fs::dir_exists(src_path)) {
    return(invisible(NULL))
  }

  # Do we need this initial detection of files?
  files <- sort(fs::path_rel(build_files(src_path), src_path))
  commands <- sort(build_commands(src_path, package, files, desc))

  # There might be additional files e.g. if another library is compiled.
  # See https://github.com/r-lib/ragg/blob/2c09f210bbd4a6df9cc710022195aa63b97eaeb3/src/Makevars.win#L17
  # We grab the files from the commands to cover that case.
  files <- build_files_from_commands(commands)

  directives <- Map(
    cmd = commands,
    file = files,
    dir = src_path,
    as_json_directive
  )

  json <- jsonlite::toJSON(unname(directives))
  json <- jsonlite::prettify(json)

  # We want a pretty file with a single trailing newline
  json <- sub("\n*$", "\n", json)

  writeLines(
    json,
    fs::path(path, "compile_commands.json"),
    sep = "",
    useBytes = TRUE
  )

  invisible(json)
}

has_compilation_db <- function(desc) {
  field <- toupper(desc$get_field(
    "Config/build/compilation-database",
    default = FALSE
  ))

  out <- as.logical(field)
  check_bool(out, arg = "Config/build/compilation-database")

  out
}

# Same pattern as in `R CMD shlib`.
FILE_PATTERN <- "\\.([cfmM]|cc|cpp|f90|f95|mm)"

build_files <- function(src_path) {
  makevars <- makevars_file(src_path)
  has_objects <- !is.null(makevars) &&
    any(grepl("^OBJECTS *=", readLines(makevars)))

  if (!has_objects) {
    # If the Makevars doesn't define custom objects, just grab all source files
    # in `src`.
    files <- dir(
      src_path,
      pattern = paste0(FILE_PATTERN, "$"),
      all.files = TRUE,
      full.names = TRUE
    )
    return(files)
  }

  # The `OBJECTS` variable should not depend on R variables, so we can inspect
  # it in isolation
  local_dir(src_path)
  pkgbuild::with_build_tools(
    out <- processx::run(
      "make",
      c(
        "-f",
        makevars,
        "-f",
        system.file("print-var.mk", package = "pkgload"),
        "print-OBJECTS"
      ),
    )
  )

  files <- strsplit(out$stdout, " ")[[1]]
  files <- fs::path(src_path, files)

  vapply(files, find_source, "")
}

# Build commands for object files take the input files in `-c` arguments that we
# extract here
build_files_from_commands <- function(commands) {
  pattern <- paste0("-c\\s+['\"]?([^\\s]+", FILE_PATTERN, ")")

  files <- regmatches(
    commands,
    regexpr(pattern, commands, perl = TRUE)
  )

  if (length(files) != length(commands)) {
    abort(
      "Expected same number of object files as compilation commands",
      .internal = TRUE
    )
  }

  sub("-c\\s+['\"]?", "", files)
}

find_source <- function(file) {
  base <- fs::path_file(fs::path_ext_remove(file))
  dir <- fs::path_dir(file)

  candidates <- dir(
    dir,
    pattern = "\\.([cfmM]|cc|cpp|f90|f95|mm)$",
    all.files = TRUE
  )
  candidates <- candidates[startsWith(candidates, paste0(base, "."))]
  n <- length(candidates)

  if (n < 1) {
    abort(sprintf("Can't find source for object file %s", file))
  }

  if (n > 1) {
    warn(sprintf(
      "Object file %s has more than one corresponding source file.\nSelected: %s\nDiscarded: %s",
      file,
      candidates[[1]],
      paste0(candidates[-1], collapse = ", ")
    ))
  }

  fs::path(dir, candidates[[1]])
}

build_commands <- function(src_path, package, files, desc) {
  rcmd <- function(...) {
    out <- pkgbuild::rcmd_build_tools(..., quiet = TRUE)

    if (out$status != 0) {
      abort(c("Can't generate compilation database.", out$stderr))
    }

    out$stdout
  }

  # `R CMD shlib` doesn't include `-I` flags for `LinkingTo` dependencies so we
  # inject these manually
  linking_to_flags <- linking_to_flags(desc)

  if (is_windows()) {
    ext <- ".dll"
  } else {
    ext <- ".so"
  }

  out <- rcmd(
    wd = src_path,
    env = c(CLINK_CPPFLAGS = linking_to_flags),
    "SHLIB",
    c(
      "--dry-run",
      "-o",
      paste0(package, ext),
      # Inject `--always-make` in make arguments to force full dry-run
      # See https://github.com/rstudio/rstudio/pull/11917
      sprintf("' --always-make %s IGNORED='", linking_to_flags),
      files
    )
  )
  out <- strsplit(out, "\n")[[1]]

  # Remove any line that doesn't look like a compilation command. Note that
  # removing the header and footer is not sufficient, some other build commands
  # might be interspersed in some cases, e.g. with rlang.
  keep <- Reduce(
    function(keep, cmp) keep | startsWith(out, cmp),
    compilers(),
    FALSE
  )

  out[keep]
}

# `R CMD config` is quite slow so we print the variables of interest by directly
# invoking make
compilers <- function() {
  if (is_windows()) {
    system_makeconf <- fs::path(R.home(), "etc", .Platform$r_arch, "Makeconf")
  } else {
    system_makeconf <- fs::path(R.home(), "etc", "Makeconf")
  }

  makevars <- c(
    system_makeconf,
    tools::makevars_site(),
    tools::makevars_user(),
    system.file("print-var.mk", package = "pkgload")
  )
  makevars <- unlist(lapply(
    makevars,
    function(var) if (length(var)) c("-f", var)
  ))

  # Add silent flag to avoid unwanted output
  makevars <- c("-s", makevars)

  # These variables are normally set by frontends but just in case
  env <- c(
    "current",
    R_INCLUDE_DIR = R.home("include"),
    R_SHARE_DIR = R.home("share")
  )

  pkgbuild::with_build_tools(
    out <- processx::run("make", c(makevars, "print-compilers"), env = env)
  )

  compilers <- strsplit(trimws(out$stdout), "\n+")[[1]]

  # Remove arguments
  compilers <- strsplit(compilers, " ")
  compilers <- vapply(compilers, function(cmp) cmp[[1]], "")

  unique(compilers)
}

as_json_directive <- function(cmd, file, dir) {
  if (!grepl(file, cmd, fixed = TRUE)) {
    abort(
      "Compilation command doesn't match object file",
      .internal = TRUE
    )
  }

  list(
    command = jsonlite::unbox(cmd),
    file = jsonlite::unbox(file),
    directory = jsonlite::unbox(dir)
  )
}

makevars_file <- function(src_path) {
  if (is_windows()) {
    if (Sys.getenv("R_ARCH") == "/x64") {
      file <- fs::path(src_path, "Makevars.ucrt")
      if (fs::file_exists(file)) {
        return(file)
      }

      file <- fs::path(src_path, "Makevars.win64")
      if (fs::file_exists(file)) {
        return(file)
      }
    }

    file <- fs::path(src_path, "Makevars.win")
    if (fs::file_exists(file)) {
      return(file)
    }
  }

  file <- fs::path(src_path, "Makevars")
  if (fs::file_exists(file)) {
    return(file)
  }

  NULL
}

linking_to_flags <- function(desc) {
  linking_to <- desc$get_field("LinkingTo", default = NULL)

  if (is.null(linking_to)) {
    return("")
  }

  # Split by comma
  linking_to <- strsplit(linking_to, " *, *", )[[1]]

  # Remove version if any
  linking_to <- strsplit(linking_to, " *\\(", )
  linking_to <- vapply(linking_to, function(pkg) pkg[[1]], "")

  paths <- vapply(
    linking_to,
    function(pkg) system.file("include", package = pkg),
    ""
  )
  paths <- paths[paths != ""]

  paste(paste0("-I\"", paths, "\""), collapse = " ")
}
