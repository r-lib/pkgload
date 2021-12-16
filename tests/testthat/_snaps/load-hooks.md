# user onLoad hooks are properly run

    Code
      (expect_warning(expect_condition(load_all("testUserLoadHookUpstream"), class = "hook_was_run_error"))
      )
    Output
      <warning/rlang_warning>
      Warning:
      Problem while running user `onLoad` hook for package testUserLoadHookUpstream.
      i The hook inherits from `namespace:testUserLoadHookError`.
      Caused by error in `fun()`:
      ! The message.

