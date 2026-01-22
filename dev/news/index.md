# Changelog

## pkgload (development version)

## pkgload 1.4.1

CRAN release: 2025-09-23

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now supports explicit `debug` option to control using debug compiler
  flags ([\#224](https://github.com/r-lib/pkgload/issues/224),
  [@assaron](https://github.com/assaron)).

- The generator of `compile_commands.json` now works with packages that
  compile extra libraries such as ragg.

- The generator of `compile_commands.json` now works with sources in
  subdirectories ([\#308](https://github.com/r-lib/pkgload/issues/308),
  [@krlmlr](https://github.com/krlmlr)).

- The generator of `compile_commands.json` now checks for existing
  `R_SHARE_DIR` and `R_INCLUDE_DIR` environment variables
  ([\#287](https://github.com/r-lib/pkgload/issues/287),
  [\#296](https://github.com/r-lib/pkgload/issues/296),
  [@TimTaylor](https://github.com/TimTaylor) and
  [@shikokuchuo](https://github.com/shikokuchuo)).

- The generator of `compile_commands.json` is now more reliable in the
  presence of extra whitespace in `make`’s output
  ([\#288](https://github.com/r-lib/pkgload/issues/288),
  [@TimTaylor](https://github.com/TimTaylor)).

- The generator of `compile_commands.json` now uses escaped double
  quotes for LinkingTo packages to ensure valid argument strings when
  parsed on Windows
  ([\#305](https://github.com/r-lib/pkgload/issues/305),
  [@tylermorganwall](https://github.com/tylermorganwall)).

## pkgload 1.4.0

CRAN release: 2024-06-28

- The `reset` argument of
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md) is
  no longer supported because preserving the namespace requires
  unlocking its environment, which is no longer possible in recent
  versions of R. It should no longer be necessary as the performance
  issues caused by resetting the namespace were resolved a while ago.

- New experimental feature for generating a `compile_commands.json` file
  after each
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md).
  This file is used by LSP servers such as clangd to provide
  intellisense features in your native files. To enable it, add this
  directive to your `DESCRIPTION` file:

      Config/build/compilation-database: true

  You’ll also want to add `compile_commands.json` and `.cache` to your
  gitignore and Rbuildignore files.

  To accomplish all these steps, feel free to use the unexported
  function `pkgload:::use_compilation_db()`. It will eventually be
  exported from the usethis package.

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now includes a link to the exact location when loading failed
  ([@olivroy](https://github.com/olivroy),
  [\#282](https://github.com/r-lib/pkgload/issues/282)).

- User onload hooks are now passed a library path.

- Fixed an error when updating packages on load
  ([@olivroy](https://github.com/olivroy),
  [\#261](https://github.com/r-lib/pkgload/issues/261)).

- Fixed a bug in
  [`shim_help()`](https://pkgload.r-lib.org/dev/reference/help.md) where
  a complex `package =` argument evaluating to `NULL` would cause an
  error ([\#266](https://github.com/r-lib/pkgload/issues/266)).

## pkgload 1.3.4

CRAN release: 2024-01-16

- On load, pkgload now sets `PKGLOAD_PARENT_TEMPDIR` to the temporary
  directory used in the current process. This provides a convenient
  place to cache results for functions used in subprocesses
  (e.g. `devtools::test()`, `devtools::document()`).

- Fixes for Debian packaging.

## pkgload 1.3.3

CRAN release: 2023-09-22

- [`dev_topic_index()`](https://pkgload.r-lib.org/dev/reference/dev_help.md)
  is now exported
  ([\#257](https://github.com/r-lib/pkgload/issues/257)).

- Fix handling of active bindings inside a package during unloading
  ([\#255](https://github.com/r-lib/pkgload/issues/255),
  [@klmr](https://github.com/klmr)).

- The `helpers` argument of `load_all` now defaults to the value
  provided for the `export_all` arguments. This makes the behaviour
  safer by default
  ([\#244](https://github.com/r-lib/pkgload/issues/244)).

- pkgload now depends unconditionally on pkgbuild
  ([\#249](https://github.com/r-lib/pkgload/issues/249)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md) no
  longer standardises version number in namespace metadata
  ([\#231](https://github.com/r-lib/pkgload/issues/231)).

- New
  [`pkg_version_raw()`](https://pkgload.r-lib.org/dev/reference/packages.md)
  to get raw package version as a string.

## pkgload 1.3.2

CRAN release: 2022-11-16

- Fixes for CRAN checks.

## pkgload 1.3.1

CRAN release: 2022-10-28

- [`dev_topic_find()`](https://pkgload.r-lib.org/dev/reference/dev_help.md)
  is now exported
  ([\#215](https://github.com/r-lib/pkgload/issues/215)).

- [`dev_help()`](https://pkgload.r-lib.org/dev/reference/dev_help.md)
  will remind you to run
  [`pkgload::load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  if no in-development packages are found
  ([\#221](https://github.com/r-lib/pkgload/issues/221)).

- Shimmed `?` now works even if you’ve renamed the documentation topic
  ([\#220](https://github.com/r-lib/pkgload/issues/220)).

- [`dev_help()`](https://pkgload.r-lib.org/dev/reference/dev_help.md)
  now works with an RStudio daily to deliver a rendered development
  documentation that includes working images and links
  ([\#228](https://github.com/r-lib/pkgload/issues/228)).

## pkgload 1.3.0

CRAN release: 2022-06-27

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now calls
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  to prompt whether to install missing packages.

  Outdated and missing dependencies are installed using pak if
  installed. If not, the remotes package is used if installed. Otherwise
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  is used as a last resort but this method does not support Remotes
  fields.

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  gains an `attach` argument set to `TRUE` by default
  ([\#209](https://github.com/r-lib/pkgload/issues/209)). If set to
  `FALSE`,
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  creates a new namespace but doesn’t create a package environment on
  the search path. In this case, it is more similar to
  [`loadNamespace()`](https://rdrr.io/r/base/ns-load.html) than to
  [`library()`](https://rdrr.io/r/base/library.html).

- Improved the way help pages are displayed in RStudio. This makes the
  behaviour within and outside RStudio consistent and fixes issues with
  Rd macros ([\#120](https://github.com/r-lib/pkgload/issues/120)).

- [`unregister()`](https://pkgload.r-lib.org/dev/reference/unload.md) is
  now exported. This is a gentler version of
  [`unload()`](https://pkgload.r-lib.org/dev/reference/unload.md) which
  removes the package from the search path, unregisters methods, and
  unregisters the namespace. However it doesn’t try to unload the
  namespace or its DLL so that dangling references keep working.

- User `onLoad` hooks are now run after exports have been populated.
  This allows the hook to use exported functions.

- The loaded namespace is now locked just before user `onLoad` hooks are
  run. This better reproduced the namespace sealing behaviour of regular
  loading.

  The package environment environment is now locked as well before both
  the user and package `onAttach` hooks are run.

- Added support for loading a .so or .dll file from the `inst` folder
  via a new
  [`library.dynam()`](https://rdrr.io/r/base/library.dynam.html) shim
  ([@ethanplunkett](https://github.com/ethanplunkett),
  [\#48](https://github.com/r-lib/pkgload/issues/48)).

- The
  [`system.file()`](https://pkgload.r-lib.org/dev/reference/system.file.md)
  shim now fails if you supply a path that starts with `inst` to better
  reproduce the behaviour with installed packages
  ([\#104](https://github.com/r-lib/pkgload/issues/104)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now imports its dependencies lazily to avoid parallel installation
  issues ([\#89](https://github.com/r-lib/pkgload/issues/89)).

- Unknown Rd macros no longer trigger a warning when building the
  package topic index
  ([\#119](https://github.com/r-lib/pkgload/issues/119)).

- `load_all(compile = TRUE)` now forces a full recompilation
  ([\#93](https://github.com/r-lib/pkgload/issues/93)).

- The advice about running [`rm()`](https://rdrr.io/r/base/rm.html) to
  remove conflicts with objects in the global environment is now
  clickable in RStudio
  ([\#199](https://github.com/r-lib/pkgload/issues/199)).

- New
  [`is_loading()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  predicate to detect whether
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md) is
  currently running
  ([\#134](https://github.com/r-lib/pkgload/issues/134)).

- [`.dynLibs()`](https://rdrr.io/r/base/library.dynam.html) is no longer
  emptied when package with no DLL is unloaded
  ([\#176](https://github.com/r-lib/pkgload/issues/176)).

- The `?` shim no longer interprets
  [`?"/"`](https://rdrr.io/r/base/Arithmetic.html) as a path
  ([\#198](https://github.com/r-lib/pkgload/issues/198)).

- rstudioapi is no longer a hard dependency of pkgload
  ([\#187](https://github.com/r-lib/pkgload/issues/187)).

- Errors thrown in user hooks are now demoted to a warning condition.
  Previously they were demoted using
  [`try()`](https://rdrr.io/r/base/try.html), making it harder to debug
  them.

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  correctly re-loads modified translations, avoiding the usual gettext
  behaviour.

## pkgload 1.2.4

CRAN release: 2021-11-30

- Lionel Henry is now the maintainer.

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  automatically registers package translations, if found.

## pkgload 1.2.3

CRAN release: 2021-10-13

- pkgload now forces all bindings on unload. This fixes errors and
  inconsistencies when dangling references force lazy bindings after
  unload or reload.

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now restores S3 methods registered by third party packages
  ([\#163](https://github.com/r-lib/pkgload/issues/163)).

- [`load_dll()`](https://pkgload.r-lib.org/dev/reference/load_dll.md)
  will now preserve the DLL name when loading instead of always using
  the package name. This allows packages to include DLL’s with different
  names ([\#162](https://github.com/r-lib/pkgload/issues/162),
  [@dfalbel](https://github.com/dfalbel)).

## pkgload 1.2.2

CRAN release: 2021-09-11

## pkgload 1.2.1

CRAN release: 2021-04-06

- [`unload()`](https://pkgload.r-lib.org/dev/reference/unload.md) no
  longer unregisters methods for generics of the package being unloaded.
  This way dangling references to generics defined in the stale
  namespace still work as expected (r-lib/vctrs#1341).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  will now work for packages that have testthat tests but do not have
  testthat installed
  ([\#151](https://github.com/r-lib/pkgload/issues/151))

- The `pkgbuild` dependency has been moved to `Suggests`, as it is only
  needed for packages with compiled code.

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  will now work for packages that have testthat tests but do not have
  testthat installed
  ([\#151](https://github.com/r-lib/pkgload/issues/151))

- `load_all(warn_conflicts = TRUE)` becomes more narrow and only warns
  when a *function* in the global environment masks a *function* in the
  package, consistent with the docs
  ([\#125](https://github.com/r-lib/pkgload/issues/125),
  [\#143](https://github.com/r-lib/pkgload/issues/143)
  [@jennybc](https://github.com/jennybc)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md) no
  longer does a comprehensive check on the `DESCRIPTION` file when
  loading, instead just checking that it exists and starts with Package
  ([\#149](https://github.com/r-lib/pkgload/issues/149),
  [@malcolmbarrett](https://github.com/malcolmbarrett))

- [`unload()`](https://pkgload.r-lib.org/dev/reference/unload.md) no
  longer warns when it can’t unload a namespace.

## pkgload 1.2.0

CRAN release: 2021-02-23

- Fix test failure in R 4.1 with regards to S4 method registration

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now preserves existing namespaces in working order. In particular, it
  doesn’t unload the package’s shared library and keeps it loaded
  instead. When reloading, a copy of the SO for the new namespace is
  loaded from a temporary location. These temporary SOs are only
  unloaded on GC and deleted from their temporary location via a weak
  reference attached to the namespace.

  This mechanism ensures that lingering references to the namespace keep
  working as expected. Consequently the namespace propagation routine
  that was added to pkgload as a workaround has been removed.

  Note that [`.Call()`](https://rdrr.io/r/base/CallExternal.html)
  invocations that pass a string symbol rather than a structured symbol
  may keep crashing, because R will look into the most recently loaded
  SO of a given name. Since symbol registration is now the norm, we
  don’t expect this to cause much trouble.

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md) no
  longer forces all bindings of a namespace to avoid lazy-load errors.
  Instead, it removes exported S3 methods from the relevant tables.

  - This improves the loading behaviour with packages that define
    objects in their namespaces lazily (e.g. with
    [`delayedAssign()`](https://rdrr.io/r/base/delayedAssign.html)).

  - This also makes
    [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
    more predictable after a method has been removed from the package.
    It is now actually removed from the generic table. It would
    previously linger until R was restarted.

- If [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  attaches testthat, it automatically suppresses conflicts.

## pkgload 1.1.0

CRAN release: 2020-05-29

- [`dev_example()`](https://pkgload.r-lib.org/dev/reference/dev_example.md)
  now works after removing an inconsistent call to
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  ([@riccardoporreca](https://github.com/riccardoporreca),
  [\#122](https://github.com/r-lib/pkgload/issues/122)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now issues a warning if exported objects conflict with objects defined
  in the global environment
  ([\#112](https://github.com/r-lib/pkgload/issues/112))

- [`run_example()`](https://pkgload.r-lib.org/dev/reference/dev_example.md)
  arguments `run` and `test` are deprecated in favor of the (hopefully)
  more clear `run_dontrun` and `run_donttest`
  ([\#107](https://github.com/r-lib/pkgload/issues/107)).

- Internal fixes for compatibility with the future 4.1.0 release.

## pkgload 1.0.2

CRAN release: 2018-10-29

- [`shim_question()`](https://pkgload.r-lib.org/dev/reference/help.md)
  now works for topics from the R base package that are passed with the
  double colon operator
  (e.g. [`base::min`](https://rdrr.io/r/base/Extremes.html))
  ([@mdequeljoe](https://github.com/mdequeljoe),
  [\#99](https://github.com/r-lib/pkgload/issues/99)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now allows using explicitly qualified, exported names in test helpers
  ([@klmr](https://github.com/klmr),
  [\#95](https://github.com/r-lib/pkgload/issues/95)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  gains a `compile` argument which controls more finely whether to
  compile the code or not. The `recompile` argument is now deprecated
  and will be removed in a future version of pkgload.

## pkgload 1.0.1

CRAN release: 2018-10-11

- [`unload()`](https://pkgload.r-lib.org/dev/reference/unload.md) now
  only removes S4 classes which were generated in the package being
  unloaded ([\#75](https://github.com/r-lib/pkgload/issues/75))

- [`help()`](https://pkgload.r-lib.org/dev/reference/help.md) will no
  longer error when trying to load package level help
  ([\#67](https://github.com/r-lib/pkgload/issues/67)).

- Trailing slashes now removed from all paths, which fixes issues on
  Windows ([\#73](https://github.com/r-lib/pkgload/issues/73)).

- [`load_dll()`](https://pkgload.r-lib.org/dev/reference/load_dll.md)
  now fixed in R-devel
  ([\#77](https://github.com/r-lib/pkgload/issues/77)).

- The help shim’s now work for `:::` inputs
  ([\#72](https://github.com/r-lib/pkgload/issues/72)).

## pkgload 1.0.0

CRAN release: 2018-07-07

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now updates imports of dependent packages when a package is reloaded
  ([\#59](https://github.com/r-lib/pkgload/issues/59)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now assigns `DESCRIPTION/Depends` to `.Depends` object of package
  environment. ([@yiufung](https://github.com/yiufung) pkgload#61)

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now attaches `testthat` if the `attach_testthat` option is `TRUE`.
  This allows
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md) to
  more closely mimic the testing environment.
  ([\#56](https://github.com/r-lib/pkgload/issues/56))

- [`check_dep_version()`](https://pkgload.r-lib.org/dev/reference/check_dep_version.md)
  and
  [`check_suggested()`](https://pkgload.r-lib.org/dev/reference/check_suggested.md)
  are now exported.

- [`check_dep_version()`](https://pkgload.r-lib.org/dev/reference/check_dep_version.md)
  now emits a warning and returns `FALSE` rather than aborting.
  ([\#47](https://github.com/r-lib/pkgload/issues/47))

- Package imports are now exported when using
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md).
  This behavior can be disabled by using
  `load_all(export_imports = FALSE)`.

- The `as.package()` and `is.package()` functions have been removed.

- [`load_code()`](https://pkgload.r-lib.org/dev/reference/load_code.md),
  [`load_data()`](https://pkgload.r-lib.org/dev/reference/load_data.md),
  [`load_dll()`](https://pkgload.r-lib.org/dev/reference/load_dll.md),
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md),
  [`parse_ns_file()`](https://pkgload.r-lib.org/dev/reference/parse_ns_file.md)
  all now take an explicit path rather than a path or a `package`
  object.

- [`imports_env()`](https://pkgload.r-lib.org/dev/reference/imports_env.md),
  [`ns_env()`](https://pkgload.r-lib.org/dev/reference/ns_env.md),
  [`pkg_env()`](https://pkgload.r-lib.org/dev/reference/pkg_env.md) and
  [`unload()`](https://pkgload.r-lib.org/dev/reference/unload.md) now
  take a package name rather than a path or a `package` object.

- [`run_example()`](https://pkgload.r-lib.org/dev/reference/dev_example.md)
  now works on R 3.1.

- [`unload()`](https://pkgload.r-lib.org/dev/reference/unload.md) now
  unloads S4 classes for packages loaded with
  [`library()`](https://rdrr.io/r/base/library.html) as well as
  [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  ([\#46](https://github.com/r-lib/pkgload/issues/46)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  gains a `helpers` option to specify whether or not to source testthat
  helpers. ([@pitakakariki](https://github.com/pitakakariki) devtools
  [\#1202](https://github.com/r-lib/pkgload/issues/1202))

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now sources the testthat helpers in the namespace environment rather
  than the package environment
  ([\#40](https://github.com/r-lib/pkgload/issues/40)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  now sets the `NOT_CRAN` environment variable when it sources testthat
  helpers. It also sets `DEVTOOLS_LOAD` to “true” so that you can check
  whether they are run during package loading.

- `dev_topic_path()` now only returns the last path found, fixing an
  error when a package has both a package function level help with the
  same name. ([\#21](https://github.com/r-lib/pkgload/issues/21))

- New function
  [`is_dev_package()`](https://pkgload.r-lib.org/dev/reference/is_dev_package.md)
  to determine if a given package has been loaded by
  [`pkgload::load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md)
  ([\#2](https://github.com/r-lib/pkgload/issues/2)).

- [`load_all()`](https://pkgload.r-lib.org/dev/reference/load_all.md) no
  longer updates the collate directive. Instead this functionality has
  been moved to `devtools::document()`.

- [`dev_help()`](https://pkgload.r-lib.org/dev/reference/dev_help.md)
  now optionally takes a character vector of packages to search within.
  This replaces `find_topic()`.

- [`dev_topic_index_reset()`](https://pkgload.r-lib.org/dev/reference/dev_help.md)
  is now exported, and allows you to reset the topic index associated
  with a given package.

- Added a `NEWS.md` file to track changes to the package.

- Initial release from code spun off from devtools
