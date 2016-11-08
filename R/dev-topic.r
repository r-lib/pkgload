# Tools for indexing package documentation by alias, and for finding
# the rd file for a given topic (alias).

rd_files <- function(pkg) {
  path_man <- file.path(pkg$path, "man")
  files <- dir(path_man, pattern = "\\.[Rr]d$", full.names = TRUE)
  names(files) <- basename(files)
  sort_ci(files)
}

dev_topic_find <- function(topic, dev_packages = NULL) {
  topic <- dev_topic_parse(topic, dev_packages)

  path <- NULL
  pkg <- NULL
  for (pkg_name in topic$pkg_names) {
    pkg <- as.package(getNamespaceInfo(pkg_name, "path"))
    path <- dev_topic_path(topic$topic, pkg = pkg)
    if (!is.null(path)) {
      return(list(
        path = path,
        pkg = pkg
      ))
    }
  }

  NULL
}

dev_topic_parse <- function(topic, dev_packages = NULL) {
  stopifnot(is.character(topic), length(topic) == 1)

  pieces <- strsplit(topic, "::")[[1]]
  if (length(pieces) == 1) {
    if (is.null(dev_packages)) {
      pkgs <- dev_packages()
    } else {
      pkgs <- dev_packages
    }
  } else {
    pkgs <- pieces[1]
    topic <- pieces[2]
  }

  list(
    topic = topic,
    pkg_names = pkgs
  )
}


dev_topic_path <- function(topic, pkg = ".") {
  pkg <- as.package(pkg)

  # First see if a man file of that name exists
  man <- file.path(pkg$path, "man", topic)
  if (file.exists(man))
    return(man)

  # Next, look in index
  index <- dev_topic_index(pkg)
  if (topic %in% names(index))
    return(file.path(pkg$path, "man", index[[topic]]))

  # Finally, try adding .Rd to name
  man_rd <- file.path(pkg$path, "man", paste0(topic, ".Rd"))
  if (file.exists(man_rd))
    return(man_rd)

  NULL
}


# Cache -------------------------------------------------------------------

dev_topic_indices <- new.env(parent = emptyenv())
dev_topic_index <- function(pkg = ".") {
  pkg <- as.package(pkg)

  if (!exists(pkg$package, dev_topic_indices)) {
    dev_topic_indices[[pkg$package]] <- build_topic_index(pkg)
  }
  dev_topic_indices[[pkg$package]]
}

#' @export
#' @rdname dev_help
#' @param pkg_name Name of package.
dev_topic_index_reset <- function(pkg_name) {
  if (exists(pkg_name, dev_topic_indices)) {
    rm(list = pkg_name, envir = dev_topic_indices)
  }

  invisible(TRUE)
}

# Topic index -------------------------------------------------------------

build_topic_index <- function(pkg = ".") {
  pkg <- as.package(pkg)
  rds <- rd_files(pkg)

  aliases <- function(path) {
    parsed <- tools::parse_Rd(path)
    tags <- vapply(parsed, function(x) attr(x, "Rd_tag")[[1]], character(1))
    unlist(parsed[tags == "\\alias"])
  }

  invert(lapply(rds, aliases))
}

invert <- function(L) {
  if (length(L) == 0) return(L)
  t1 <- unlist(L)
  names(t1) <- rep(names(L), lapply(L, length))
  tapply(names(t1), t1, c)
}
