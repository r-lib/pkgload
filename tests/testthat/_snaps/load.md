# warn_if_conflicts warns for conflicts and both objects are functions

    Code
      (expect_warning(warn_if_conflicts("pkg", e1, e2)))
    Output
      <warning/pkgload::conflict>
      Warning:
      -- Conflicts -------------------------------------------------- pkg conflicts --
      `x` foo() masks `pkg::foo()`.
      i Did you accidentally source a file rather than using `load_all()`?
        Run `rm(list = c("foo"))` to remove the conflicts.

