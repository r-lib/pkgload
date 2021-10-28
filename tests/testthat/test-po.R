test_that("translation domain correctly loaded", {
  load_all(test_path("testTranslations"))
  withr::defer(unload("testTranslations"))

  withr::local_envvar(LANGUAGE = "fr")
  expect_equal(hello(), "Bonjour")
})
