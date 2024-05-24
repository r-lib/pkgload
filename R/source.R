source_many <- function(files, encoding = "UTF-8", envir = parent.frame()) {
  stopifnot(is.character(files))
  stopifnot(is.environment(envir))

  oop <- options(
    keep.source = TRUE,
    show.error.locations = TRUE,
    topLevelEnvironment = as.environment(envir))
  on.exit(options(oop))

  for (file in files) {
    try_fetch(
      source_one(file, encoding, envir = envir),
      error = function(cnd) {
        path <- file.path(basename(dirname(file)), basename(file))
        is_parse_error <- identical(as.character(cnd$call[1]), "parse")

        if (is_parse_error) {
          # Tweak base message to be shorter and add link to src location.
          location <- conditionMessage(cnd)
          # extract :<line>:<col> in base message.
          line_col <- regmatches(location, m = regexpr("\\:\\d+\\:\\d+", location))
          if (length(line_col) > 0) {
            # Tweak parent message.
            path_show <- paste0(path, line_col)
            # tweak parse() message to include an hyperlink.
            # Replace full path by relative path + hyperlink
            path_hyperlink <- cli::format_inline(paste0("At {.file ", path_show, "}:"))

            cnd$message <- sub(
              paste0("^.*", path_show, "\\:"),
              path_hyperlink,
              cnd$message
            )
            # only need for basename in pkgload message, since hyperlink
            # is now included in parent message
            msg <- paste0("Failed to load {.val {basename(path)}}")

          }
        } else {
          msg <- paste0("Failed to load {.file {path}}")
        }
        cli::cli_abort(msg, parent = cnd, call = quote(load_all()))
      }
    )
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
