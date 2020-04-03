context("classif.C5.0")

test_that("autotest", {
  learner = LearnerClassifC5.0$new()
  learner$param_set$values <- list(trials = 30L)
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
