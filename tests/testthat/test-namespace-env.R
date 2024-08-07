test_that("respects version separator", {
  ns <- create_ns_env(test_path("testVersionSep"))
  defer(unregister_namespace("testVersionSep"))

  expect_equal(getNamespaceInfo(ns, "spec")[["version"]], "0.0.0-9000")
})
