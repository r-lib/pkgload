load_po <- function(package, path) {
  po_path <- file.path(path, "inst", "po")
  if (!file.exists(po_path)) {
    return()
  }

  # Reset cache to avoid gettext() retrieving cached value
  # See <https://bugs.r-project.org/show_bug.cgi?id=18055> for details
  bindtextdomain("reset", withr::local_tempdir())

  bindtextdomain(paste0("R-", package), po_path) # R level messages
  bindtextdomain(package, po_path) # C level messages

  invisible()
}
