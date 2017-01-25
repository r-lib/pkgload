source_many <- function(files, encoding, envir = parent.frame()) {
  stopifnot(is.character(files))
  stopifnot(is.environment(envir))

  oop <- options(
    keep.source = TRUE,
    show.error.locations = TRUE,
    topLevelEnvironment = as.environment(envir))
  on.exit(options(oop))

  for (file in files) {
    source_one(file, encoding, envir = envir)
  }
  invisible()
}

source_one <- function(file, encoding, envir = parent.frame()) {
  stopifnot(file.exists(file))
  stopifnot(is.environment(envir))

  lines <- read_lines_enc(file, file_encoding = encoding)
  srcfile <- srcfilecopy(file, lines, file.info(file)[1, "mtime"],
    isFile = TRUE)
  exprs <- parse(text = lines, n = -1, srcfile = srcfile)

  n <- length(exprs)
  if (n == 0L) return(invisible())

  for (i in seq_len(n)) {
    eval(exprs[i], envir)
  }
  invisible()
}
