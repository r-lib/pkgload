# shim_question behaves the same as utils::? for nonexistent objects

    Code
      (expect_error(utils::`?`(foofoo(123))))
    Output
      <simpleError in .helpForCall(topicExpr, parent.frame()): no methods for 'foofoo' and no documentation for it as a function>
    Code
      (expect_error(shim_question(foofoo(123))))
    Output
      <simpleError in .helpForCall(topicExpr, parent.frame()): no methods for 'foofoo' and no documentation for it as a function>

# dev_help gives clear error if no packages loaded

    Code
      dev_help("foo")
    Condition
      Error in `dev_help()`:
      ! Can't find development documentation because no in-development packages loaded.
      i Do you need to run `pkgload::load_all()`?

# complex expressions are checked

    Code
      (expect_error(shim_help({
        foo
        bar
      }), "must be a name"))
    Output
      <error/rlang_error>
      Error in `shim_help()`:
      ! `topic` must be a name.

