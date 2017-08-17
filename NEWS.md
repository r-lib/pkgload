# pkgload 0.0.0.9000

* `check_dep_version()` and `check_suggested()` are now exported.

* `check_dep_version()` now emits a warning and returns `FALSE` rather than
  aborting. (#47)

* Package imports are now exported when using `load_all()`. This behavior can
  be disabled by using `load_all(export_imports = FALSE)`.

* The `as.package()` and `is.package()` functions have been removed.

* `load_code()`, `load_data()`, `load_dll()`, `load_all()`, `parse_ns_file()`
  all now take an explicit path rather than a path or a `package` object.

* `imports_env()`, `ns_env()`, `pkg_env()` and `unload()` now take a package
  name rather than a path or a `package` object.

* `run_example()` now works on R 3.1.

* `unload()` now unloads S4 classes for packages loaded with `library()` as
  well as `load_all()` (#46).

* `load_all()` gains a `helpers` option to specify whether or not to
  source testthat helpers. (@pitakakariki devtools #1202)

* `load_all()` now sources the testthat helpers in the namespace environment
  rather than the package environment (#40).

* `load_all()` now sets the `NOT_CRAN` environment variable when it
  sources testthat helpers. It also sets `DEVTOOLS_LOAD` to "true" so
  that you can check whether they are run during package loading.

* `dev_topic_path()` now only returns the last path found, fixing an error
  when a package has both a package function level help with the same name.
  (#21)

* New function `is_dev_package()` to determine if a given package has been loaded
  by `pkgload::load_all()` (#2).

* `load_all()` gains a `recollate` argument and roxygen2 is now a Suggested
  rather than required dependency. (#4)

* `dev_help()` now optionally takes a character vector of packages to
  search within.  This replaces `find_topic()`.

* `dev_topic_index_reset()` is now exported, and allows you to reset
  the topic index associated with a given package.

* Added a `NEWS.md` file to track changes to the package.
