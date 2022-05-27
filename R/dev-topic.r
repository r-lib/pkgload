# Tools for indexing package documentation by alias, and for finding
# the rd file for a given topic (alias).

rd_files <- function(path) {
  path <- pkg_path(path)
  path_man <- package_file("man", path = path)
  files <- dir(path_man, pattern = "\\.[Rr]d$", full.names = TRUE)
  names(files) <- basename(files)
  sort_ci(files)
}

dev_topic_find <- function(topic, dev_packages = NULL) {
  topic <- dev_topic_parse(topic, dev_packages)

  path <- NULL
  pkg <- NULL
  for (pkg_name in topic$pkg_names) {
    path <- dev_topic_path(topic$topic,
                           path = ns_path(pkg_name))
    if (!is.null(path)) {
      return(list(
        path = path,
        pkg = pkg_name
      ))
    }
  }

  NULL
}

dev_topic_parse <- function(topic, dev_packages = NULL) {
  stopifnot(is_string(topic))

  pieces <- strsplit(topic, ":::?")[[1]]
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


dev_topic_path <- function(topic, path = ".") {
  path <- pkg_path(path)

  # First see if a man file of that name exists
  man <- package_file("man", topic, path = path)
  if (file.exists(man))
    return(man)

  # Next, look in index
  index <- dev_topic_index(path)
  if (topic %in% names(index))
    return(package_file("man", last(index[[topic]]), path = path))

  # Finally, try adding .Rd to name
  man_rd <- package_file("man", paste0(topic, ".Rd"), path = path)
  if (file.exists(man_rd))
    return(man_rd)

  NULL
}


# Cache -------------------------------------------------------------------

dev_topic_indices <- new.env(parent = emptyenv())
dev_topic_index <- function(path = ".") {
  path <- pkg_path(path)
  package <- pkg_name(path)

  if (!exists(pkg_name(path), dev_topic_indices)) {
    dev_topic_indices[[package]] <- build_topic_index(path)
  }
  dev_topic_indices[[package]]
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

build_topic_index <- function(path = ".") {
  path <- pkg_path(path)

  macros <- load_rd_macros(path)
  rds <- rd_files(path)

  aliases <- function(path) {
    parsed <- tools::parse_Rd(path, macros = macros)
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
