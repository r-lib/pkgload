local_load_all_quiet()

test_that("translation domain correctly loaded", {
  skip_if_c_locale()

  load_all(test_path("testTranslations"))
  withr::defer(unload("testTranslations"))

  expect_equal(with_language("fr", hello()), "Bonjour")

  load_all(test_path("testTranslations"))
  expect_equal(length(temp_po_dirs("testTranslations")), 1)
})

test_that("modified translations are correctly reloaded", {
  skip_if_c_locale()

  pkg <- withr::local_tempdir()
  file.copy(dir(test_path("testTranslations"), full.names = TRUE), pkg, recursive = TRUE)

  # Load package and generate translation
  load_all(pkg)
  withr::defer(unload("testTranslations"))
  with_language("fr", hello())

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

  # Re-load and re-translate
  load_all(pkg)
  expect_equal(with_language("fr", hello()), "Salut")
})
