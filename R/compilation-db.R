# Should these tools move to pkgbuild?

generate_db <- function(path = ".") {
  rlang::check_installed("jsonlite")

  path <- pkg_path(path)
  package <- pkg_name(path)

  src_path <- fs::path(path, "src")

  if (!fs::dir_exists(src_path)) {
    return(invisible(NULL))
  }

  files <- build_files(src_path)
  commands <- build_commands(src_path, package, files)
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
    "Config/devtools/compilation-database",
    default = FALSE
  ))

  out <- as.logical(field)
  check_bool(out, arg = "Config/devtools/compilation-database")
  
  out
}

build_files <- function(src_path) {
  makevars <- makevars_file(src_path)
  has_objects <- !is.null(makevars) && any(grepl("^OBJECTS *=", readLines(makevars)))

  if (has_objects) {
    files <- stop("todo")
  } else {
    # Same pattern as in `R CMD shlib`
    files <- dir(src_path, pattern = "\\.([cfmM]|cc|cpp|f90|f95|mm)$", all.files = TRUE)
  }

  files
}

build_commands <- function(src_path, package, files) {
  rcmd <- function(...) {
    out <- pkgbuild::rcmd_build_tools(..., quiet = TRUE)

    if (out$status != 0) {
      abort(c("Can't generate compilation database.", out$stderr))
    }

    out$stdout
  }

  out <- rcmd(
    wd = src_path,
    "shlib",
    c(
      "--dry-run",
      "-o", paste0(package, ".so"),
      # Inject `--always-make` in make arguments to force full dry-run
      # See https://github.com/rstudio/rstudio/pull/11917
      "' --always-make IGNORED='",
      files
    )
  )
  out <- strsplit(out, "\n")[[1]]

  cc <- rcmd(
    wd = src_path,
    "config",
    "CC"
  )
  stopifnot(is_string(cc), nchar(cc) > 1)

  # Remove trailing newline
  cc <- substr(cc, 1, nchar(cc) - 1);

  # Remove any line that doesn't look like a compilation command. Note that
  # removing the header and footer is not sufficient, some other build commands
  # might be interspersed in some cases, e.g. with rlang.
  commands <- out[startsWith(out, cc)]

  if (length(commands) != length(files)) {
    abort(
      "Expected same number of compilation commands as object files",
      .internal = TRUE
    )
  }

  commands
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
