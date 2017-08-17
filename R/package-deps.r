#' Parse package dependency strings.
#'
#' @param string to parse. Should look like `"R (>= 3.0), ggplot2"` etc.
#' @return list of two character vectors: `name` package names,
#'   and `version` package versions. If version is not specified,
#'   it will be stored as NA.
#' @keywords internal
#' @export
#' @examples
#' parse_deps("httr (< 2.1),\nRCurl (>= 3)")
#' # only package dependencies are returned
#' parse_deps("utils (== 2.12.1),\ntools,\nR (>= 2.10),\nmemoise")
parse_deps <- function(string) {
  if (is.null(string)) return()
  stopifnot(is.character(string), length(string) == 1)
  if (grepl("^\\s*$", string)) return()

  pieces <- strsplit(string, "[[:space:]]*,[[:space:]]*")[[1]]

  # Get the names
  names <- gsub("\\s*\\(.*?\\)", "", pieces)
  names <- gsub("^\\s+|\\s+$", "", names)

  # Get the versions and comparison operators
  versions_str <- pieces
  have_version <- grepl("\\(.*\\)", versions_str)
  versions_str[!have_version] <- NA

  compare  <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
  versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)

  # Check that non-NA comparison operators are valid
  compare_nna   <- compare[!is.na(compare)]
  compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", "<")
  if(!all(compare_valid)) {
    stop("Invalid comparison operator in dependency: ",
      paste(compare_nna[!compare_valid], collapse = ", "))
  }

  deps <- data.frame(name = names, compare = compare,
    version = versions, stringsAsFactors = FALSE)

  # Remove R dependency
  deps[names != "R", ]
}


#' Check that the version of an imported package satisfies the requirements
#'
#' @param dep_name The name of the package with objects to import
#' @param dep_ver The version of the package, this includes the specified
#'   comparison operator.
#' @export
#' @keywords internal
check_dep_version <- function(dep_name, dep_ver = "*") {
  if (dep_name == "R") {
    return(TRUE)
  }

  if (!requireNamespace(dep_name, quietly = TRUE)) {
    warning("Dependency package ", dep_name, " not available.")
    return(FALSE)
  }
  if (dep_ver == "*") {
    return(TRUE)
  }

  pieces <- strsplit(dep_ver, "[[:space:]]+")[[1]]
  dep_compare <- pieces[[1]]
  dep_ver <- pieces[[2]]

  compare <- match.fun(dep_compare)
  if (!compare(
      as.numeric_version(getNamespaceVersion(dep_name)),
      as.numeric_version(dep_ver))) {

    warning("Need ", dep_name, " ", dep_compare,
      " ", dep_ver,
      " but loaded version is ", getNamespaceVersion(dep_name))
  }

  return(TRUE)
}
