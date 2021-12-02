test_that("importing an S4 exported by another pkg with export_all = FALSE", {
  load_all("testS4export", export_all = FALSE)

  # this used to crash with error: 
  # class "class_to_export" is not exported by 'namespace:testS4export'
  load_all("testS4import", export_all = FALSE)
  expect_true(isClassDef(getClass("derived")))

  load_all("testS4import", export_all = FALSE)

  cl <- getClass("derived")
  expect_true(isClassDef(cl))

  expect_s4_class(cl@contains$class_to_export, "SClassExtension")
  expect_equal(cl@contains$class_to_export@distance, 1)

  expect_s4_class(cl@contains$foo, "SClassExtension")
  expect_equal(cl@contains$foo@distance, 2)

  # cleanup
  unload('testS4import')
  unload('testS4export')
})
