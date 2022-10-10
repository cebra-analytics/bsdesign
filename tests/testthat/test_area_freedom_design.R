context("AreaFreedomDesign")

test_that("initializes with context and valid parameters", {
  expect_error(freedom_design <- AreaFreedomDesign(
    context = Context("test"),
    detected = "invalid",
    pr_detect = NULL,
    pr_persist = 1,
    iterations = NULL),
    paste("The temporal detected parameter should be a logical vector with",
          "length >= 1."))
  expect_error(freedom_design <- AreaFreedomDesign(
    context = Context("test"),
    detected = rep(1,3),
    pr_detect = 2,
    pr_persist = 1,
    iterations = NULL),
    paste("The probability of detection parameter must be numeric, >= 0,",
          "and <= 1."))
  expect_error(freedom_design <- AreaFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = 1,
    pr_persist = 2,
    iterations = NULL),
    paste("The probability of persistence parameter must be numeric, >= 0,",
          "and <= 1."))
  expect_error(freedom_design <- AreaFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = 1,
    pr_persist = 1,
    iterations = 0),
    "The iterations parameter must be numeric and >= 1.")
  expect_silent(freedom_design <- AreaFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = 1,
    pr_persist = 1,
    iterations = 3))
  expect_is(freedom_design, "AreaFreedomDesign")
  expect_null(freedom_design$get_evidence())
  expect_null(freedom_design$get_iterations())
})
