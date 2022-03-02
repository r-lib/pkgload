test_that("multiplication works", {
  expect_snapshot(error = TRUE, {
    source_many(test_path("testSource", c("a.R", "b.R")))
  })
})
