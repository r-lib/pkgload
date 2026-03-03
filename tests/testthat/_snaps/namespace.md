# unload() removes package environments from search

    Code
      (expect_error(asNamespace("testNamespace")))
    Output
      <packageNotFoundError in loadNamespace(name): there is no package called 'testNamespace'>

