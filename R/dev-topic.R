# Tools for finding the Rd file for a given topic (alias). The alias index
# is maintained by rdtools, which caches it per package until it is reset
# with `dev_topic_index_reset()` (e.g. by `devtools::document()`).

#' @rdname dev_help
#' @export
dev_topic_find <- function(topic, dev_packages = NULL) {
  stopifnot(is_string(topic))

  parsed <- rdtools::topic_split(topic)
  topic <- parsed$topic

  # Only search in-development packages, so that a qualified topic like
  # `stats::mean` falls through to `utils::help()`.
  packages <- parsed$package %||% dev_packages %||% dev_packages()
  packages <- intersect(packages, dev_packages())

  found <- rdtools::topic_find(topic, packages = packages)
  if (is.null(found)) {
    return(NULL)
  }

  path <- rdtools::topic_rd_path(topic, found$package)
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
