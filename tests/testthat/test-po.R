local_load_all_quiet()

test_that("translation domain correctly loaded", {
  load_all(test_path("testTranslations"))
  withr::defer(unload("testTranslations"))

  expect_equal(withr::with_envvar(list(LANGUAGE = "fr"), hello()), "Bonjour")
})

test_that("modifed translations are correctly reloaded", {
  pkg <- withr::local_tempdir()
  file.copy(dir(test_path("testTranslations"), full.names = TRUE), pkg, recursive = TRUE)

  load_all(test_path(pkg))
  withr::defer(unload("testTranslations"))

  # Modify .po file
  po_path <- file.path(pkg, "po", "R-fr.po")
  po <- readLines(po_path)
  po[length(po)] <- "msgstr \"Salut\""
  writeLines(po, po_path)
  # Update .mo
  mo_path <- file.path(pkg, "inst/po/fr/LC_MESSAGES/R-testTranslations.mo")
  out <- system(paste0("msgfmt -c --statistics -o", mo_path, " ", po_path), ignore.stderr = TRUE)
  if (out != 0) {
    stop("Failed to run msgfmt")
  }

  load_all(pkg)
  expect_equal(withr::with_envvar(list(LANGUAGE = "fr"), hello()), "Salut")
})
