# pkgload 0.0.0.9000

* `dev_topic_path()` now only returns the last path found, fixing an error
  when a package has both a package function level help with the same name.
  (#21)

* `load_all()` gains a `recollate` argument and roxygen2 is now a Suggested
  rather than required dependency. (#4)

* `dev_help()` now optionally takes a character vector of packages to
  search within.  This replaces `find_topic()`.
  
* `dev_topic_index_reset()` is now exported, and allows you to reset
  the topic index associated with a given package.

* Added a `NEWS.md` file to track changes to the package.



