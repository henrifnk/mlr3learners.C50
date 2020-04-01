library(mlr3learners.C50)

test_that("classif.C5.0", {
  learner = lrn("classif.C5.0")
  fun = C50::C5.0
  exclude = c(
    # Examples how to exclude certain parameters. Always comment why a parameter
    # was excluded!
    "x" # handled via mlr3
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0("
Missing parameters:
",
    paste0("- '", ParamTest$missing,"'", collapse = "
")))
})
