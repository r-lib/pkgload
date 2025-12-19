source_many <- function(files, encoding = "UTF-8", envir = parent.frame()) {
  stopifnot(is.character(files))
  stopifnot(is.environment(envir))

  local_options(
    keep.source = TRUE,
    show.error.locations = TRUE,
    topLevelEnvironment = as.environment(envir)
  )

  for (file in files) {
    try_fetch(
      source_one(file, encoding, envir = envir),
      error = function(cnd) handle_source_error(cnd, file)
    )
  }

  invisible()
}

source_one <- function(file, encoding, envir = parent.frame()) {
  stopifnot(file.exists(file))
  stopifnot(is.environment(envir))

  lines <- read_lines_enc(file, file_encoding = encoding)
  srcfile <- srcfilecopy(
    file,
    lines,
    file.info(file)[1, "mtime"],
    isFile = TRUE
  )

  ark_annotate_source <- env_get(baseenv(), ".ark_annotate_source", default = NULL)
  if (!is.null(ark_annotate_source)) {
    # Just to be sure, but should already be normalized
    file <- normalizePath(file, mustWork = TRUE)

    # Ark expects URIs
    uri <- paste0("file://", file)

    lines <- ark_annotate_source(uri, paste_line(lines)) %||% lines
  }

  withCallingHandlers(
    exprs <- parse(text = lines, n = -1, srcfile = srcfile),
    error = function(cnd) handle_parse_error(cnd, file)
  )

  for (expr in exprs) {
    eval(expr, envir)
  }

  invisible()
}

handle_source_error <- function(cnd, file) {
  path <- file.path(basename(dirname(file)), basename(file))
  msg <- paste0("Failed to load {.file {path}}")
  cli::cli_abort(msg, parent = cnd, call = quote(load_all()))
}

handle_parse_error <- function(cnd, file) {
  path <- file.path(basename(dirname(file)), basename(file))

  # Tweak base message to be shorter and add link to src location.
  msg <- conditionMessage(cnd)

  # Extract :<line>:<col> in base message.
  location <- regmatches(msg, m = regexpr("\\:\\d+\\:\\d+", msg))

  if (length(location) == 0) {
    return(zap())
  }

  suffixed_path <- paste0(path, location)

  # Tweak parse() message to include an hyperlink.
  # Replace full path by relative path + hyperlink
  path_hyperlink <- cli::format_inline(paste0(
    "At {.file ",
    suffixed_path,
    "}:"
  ))
  msg <- sub(
    paste0("^.*", suffixed_path, "\\:"),
    path_hyperlink,
    msg
  )

  abort(msg, call = conditionCall(cnd))
}
