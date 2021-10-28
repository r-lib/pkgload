load_po <- function(package, path) {
  po_path <- file.path(path, "inst", "po")
  if (!file.exists(po_path)) {
    return()
  }

  bindtextdomain(paste0("R-", package), po_path) # R level messages
  bindtextdomain(package, po_path) # C level messages

  invisible()
}
