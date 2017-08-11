#' Load a compiled DLL
#'
#' @param path Path to a package, or within a package.
#' @keywords programming
#' @name load_dll
#' @usage load_dll(path = ".")
#' @export
onload_assign("load_dll", {
  for_loop <-
    modify_lang(
      f = function(x)
        if (comp_lang(x, quote(library.dynam()), 1)) {
          quote(library.dynam2(path, lib))
        } else {
          x
        },
        extract_lang(body(loadNamespace),
          comp_lang, y = quote(for (i in seq_along(dynLibs)) NULL), idx = 1:3))

  ## The code below taken directly from base::loadNamespace
  ## https://github.com/wch/r-source/blob/tags/R-3-3-0/src/library/base/R/namespace.R#L466-L485
  ## except for the call to library.dynam2, which is a special version of
  ## library.dynam

  load_dll <- function(path = ".") {
    package <- pkg_name(path)

    env <- ns_env(package)
    nsInfo <- parse_ns_file(path)

    dlls <- list()
    dynLibs <- nsInfo$dynlibs

    !! for_loop
    addNamespaceDynLibs(env, nsInfo$dynlibs)

    invisible(dlls)
  }

  load_dll <- expr_interp(load_dll)
  fn_env(load_dll) <- rlang::ns_env("pkgload")

  load_dll
})

# Return a list of currently loaded DLLs from the package
loaded_dlls <- function(package) {
  libs <- .dynLibs()
  matchidx <- vapply(libs, "[[", character(1), "name") == package
  libs[matchidx]
}

# This is a replacement for base::library.dynam, with a slightly different
# call interface. The original requires that the name of the package is the
# same as the directory name, which isn't always the case when loading with
# devtools. This version allows them to be different, and also searches in
# the src/ directory for the DLLs, instead of the libs/$R_ARCH/ directory.
library.dynam2 <- function(path = ".", lib = "") {
  path <- pkg_path(path)

  dllname <- paste(lib, .Platform$dynlib.ext, sep = "")
  dllfile <- package_file("src", dllname, path = path)

  if (!file.exists(dllfile))
    return(invisible())

  # # The loading and registering of the dll is similar to how it's done
  # # in library.dynam.
  dllinfo <- dyn.load(dllfile)

  # Register dll info so it can be unloaded with library.dynam.unload
  .dynLibs(c(.dynLibs(), list(dllinfo)))

  return(dllinfo)
}


# This is taken directly from base::loadNamespace()
# https://github.com/wch/r-source/blob/tags/R-3-3-0/src/library/base/R/namespace.R#L270-L273
onload_assign("addNamespaceDynLibs",
  eval(extract_lang(body(loadNamespace),
      comp_lang, y = quote(addNamespaceDynLibs <- NULL), idx = 1:2)[[3]]))

# This is taken directly from base::loadNamespace
# https://github.com/wch/r-source/blob/tags/R-3-3-0/src/library/base/R/namespace.R#L287-L308
# The only change is the line used get the package name
onload_assign("assignNativeRoutines", {
  f <- eval(
    extract_lang(body(loadNamespace),
      comp_lang, y = quote(assignNativeRoutines <- NULL), idx = 1:2)[[3]])
  body(f) <- as.call(append(after = 1,
      as.list(body(f)),
      quote(package <- methods::getPackageName(env))))
  f
})
