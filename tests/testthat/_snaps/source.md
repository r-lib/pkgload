# multiplication works

    Code
      source_many(test_path("testSource", c("a.R", "b.R")))
    Condition
      Error in `load_all()`:
      ! Failed to load 'testSource/b.R'
      Caused by error in `parse()`:
      ! testSource/b.R:2:0: unexpected end of input
      1: b <-
         ^

