#' Load R code.
#'
#' Sources all `.R`/`.r` files in the `R/` directory, storing results into
#' the package namespace.
#'
#' @inheritParams load_all
#' @keywords programming
#' @export
load_code <- function(path = ".", quiet = NULL) {
  quiet <- load_all_quiet(quiet, "load_code")

  path <- pkg_path(path)
  package <- pkg_name(path)
  encoding <- pkg_desc(path)$get("Encoding")

  # Set encoding to ASCII if it is not explicitly defined
  if (is.na(encoding)) {
    encoding <- "ASCII"
  }

  env <- ns_env(package)

  r_files <- find_code(path, quiet = quiet)
  paths <- changed_files(r_files)
  if (length(paths) == 0L) return()

  success <- FALSE
  cleanup <- function() {
    if (success) return()
    clear_cache()
    unload(package)
  }
  defer(cleanup())

  local({
    local_dir(path)
    source_many(paths, encoding, env)
  })
  success <- TRUE

  invisible(r_files)
}

# Find all R files in given directory.
find_code <- function(path = ".", quiet = FALSE) {
  path_r <- package_file("R", path = path)

  r_files <- local({
    local_collate("C")
    tools::list_files_with_type(path_r, "code", full.names = TRUE)
  })

  collate <- pkg_desc(path)$get_collate()

  if (length(collate) > 0) {
    # `r_files` have full paths, so add the package path to the collated files as
    # well.
    collate <- file.path(path_r, collate)

    missing <- setdiff(collate, r_files)
    if (!quiet && length(missing) > 0) {
      cli::cli_inform(c("!" = "Skipping missing files: {.file {missing}}"))
    }
    collate <- setdiff(collate, missing)

    extra <- setdiff(r_files, collate)
    if (!quiet && length(extra) > 0) {
      cli::cli_inform(c(
        "!" = "Adding files missing in collate: {.file {extra}}"
      ))
    }

    r_files <- union(collate, r_files)
  }
  r_files
}
