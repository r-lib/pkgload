# Tools for finding the Rd file for a given topic (alias). The alias index
# is maintained by rdtools, which caches it per package until it is reset
# with `dev_topic_index_reset()` (e.g. by `devtools::document()`).

#' @rdname dev_help
#' @export
dev_topic_find <- function(topic, dev_packages = NULL) {
  stopifnot(is_string(topic))

  parsed <- rdtools::topic_parse(topic)
  topic <- parsed$topic

  # Don't interpret the division operator as a path (#198)
  if (is_string(topic, "/")) {
    return(NULL)
  }

  # Only search in-development packages, so that a qualified topic like
  # `stats::mean` falls through to `utils::help()`.
  packages <- parsed$package %||% dev_packages %||% dev_packages()
  packages <- intersect(packages, dev_packages())

  found <- rdtools::topic_find(topic, packages = packages)
  if (is.null(found)) {
    return(NULL)
  }

  # rdtools reports the Rd file without its extension; recover the path,
  # trying both `.Rd` and `.rd`. The returned path might not exist if the
  # cached index is stale (e.g. a topic moved to a different file); the caller
  # is responsible for resetting the index and retrying in that case.
  names <- paste0(found$file, c(".Rd", ".rd"))
  paths <- package_file("man", names, path = ns_path(found$package))
  existing <- paths[file.exists(paths)]
  path <- if (length(existing) > 0) existing[[1]] else paths[[1]]

  list(path = path, pkg = found$package)
}

#' @rdname dev_help
#' @param path Path to package.
#' @export
dev_topic_index <- function(path = ".") {
  rdtools::pkg_topics(pkg_path(path))
}

#' @rdname dev_help
#' @param pkg_name Name of package.
#' @export
dev_topic_index_reset <- function(pkg_name) {
  rdtools::pkg_cache_reset(pkg_name)
  invisible(TRUE)
}
