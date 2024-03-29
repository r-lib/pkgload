load_po <- function(package, path) {
  po_path <- file.path(path, "inst", "po")
  if (!file.exists(po_path)) {
    return()
  }

  # Clean up previous copies
  unlink(temp_po_dirs(package), recursive = TRUE, force = TRUE)

  # Create new copy of translations in temp dir
  tmp <- tempfile(temp_po_prefix(package))
  dir.create(tmp, showWarnings = FALSE)
  tmp_po <- file.path(tmp, "po")
  file.copy(po_path, tmp, recursive = TRUE)

  bindtextdomain(paste0("R-", package), tmp_po) # R level messages
  bindtextdomain(package, tmp_po) # C level messages

  invisible()
}

temp_po_prefix <- function(package) {
  paste0("pkgload-po-", package, "-")
}

temp_po_dirs <- function(package) {
  dir(tempdir(), paste0("^", temp_po_prefix(package)), full.names = TRUE)
}
