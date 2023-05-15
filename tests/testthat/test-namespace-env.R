test_that("respects version seperator", {
  ns <- create_ns_env(test_path("testVersionSep"))
  withr::defer(unregister_namespace("testVersionSep"))

  expect_equal(getNamespaceInfo(ns, "spec")[["version"]], "0.0.0-9000")
})
