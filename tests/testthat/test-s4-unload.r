context("s4-unload")

# Returns a named vector of this class's superclasses.
# Results are sorted so they can be compared easily to a vector.
# A contains B  ==  A is a superclass of B
get_superclasses <- function(class) {
  superclasses <- vapply(getClass(class)@contains, slot, "superClass",
    FUN.VALUE = character(1))

  sort(unname(superclasses))
}

# Returns a named vector of this class's subclasses
# Results are sorted so they can be compared easily to a vector.
# A extends B  ==  A is a subclass of B
get_subclasses <- function(class) {
  subclasses <- vapply(getClass(class)@subclasses, slot, "subClass",
    FUN.VALUE = character(1))

  sort(unname(subclasses))
}


test_that("loading and reloading s4 classes", {
  run_tests <- function() {
    # Check class hierarchy
    expect_equal(get_superclasses("A"), c("AB", "AOrNull", "mle2A", "mleA"))
    expect_equal(get_subclasses("AB"), c("A", "B"))
    expect_equal(get_superclasses("mle2"), c("mle", "mle2A", "mleA"))
    expect_equal(get_subclasses("mleA"), c("A", "mle", "mle2"))
    expect_equal(get_subclasses("mle2A"), c("A", "mle2"))
    expect_equal(get_subclasses("AOrNull"), c(".NULL", "A", "NULL"))
    expect_equal(get_subclasses("BOrNull"), c(".NULL", "B", "NULL"))

    # Check that package is registered correctly
    expect_equal(getClassDef("A")@package, "testS4union")
    expect_equal(getClassDef("AB")@package, "testS4union")
    expect_equal(getClassDef("mle2")@package, "testS4union")
    expect_equal(getClassDef("AOrNull")@package, "testS4union")
    expect_equal(getClassDef("BOrNull")@package, "testS4union")

    # Unloading shouldn't result in any errors or warnings
    expect_warning(unload("testS4union"), NA)

    # Check that classes are unregistered
    expect_true(is.null(getClassDef("A")))
    expect_true(is.null(getClassDef("B")))
    expect_true(is.null(getClassDef("AB")))
    expect_true(is.null(getClassDef("AorNULL")))
    expect_true(is.null(getClassDef("BorNULL")))
  }

  load_all("testS4union")
  run_tests()

  # Load again and repeat tests --------------------------------------------
  load_all("testS4union")

  run_tests()

  # Install package then load and run tests
  withr::with_temp_libpaths({
    install.packages("testS4union", repos = NULL, type = "source", quiet = TRUE)
    library("testS4union")
    load_all("testS4union")
    run_tests()
  })

  # Loading again shouldn't result in any errors or warnings
  expect_warning(load_all("testS4union", reset = FALSE), NA)

  unload("testS4union")
  unloadNamespace("stats4")   # This was imported by testS4union

  # Check that classes are unregistered
  # This test on A fails for some bizarre reason - bug in R? But it doesn't
  # to cause any practical problems.
  expect_true(is.null(getClassDef("A")))
  expect_true(is.null(getClassDef("B")))
  expect_true(is.null(getClassDef("AB")))
})
