context("BayesianFreedomDesign")

test_that("initializes with context and valid parameters", {
  expect_error(freedom_design <- BayesianFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = NULL,
    pr_persist = 1,
    pr_freedom = 2,
    iterations = NULL,
    confidence = NULL),
    paste("The prior probability of freedom parameter must be numeric, >= 0,",
          "and <= 1."))
  expect_error(freedom_design <- BayesianFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = NULL,
    pr_persist = 1,
    pr_freedom = 0.5,
    pr_intro = 2,
    iterations = NULL,
    confidence = NULL),
    paste("The probability of introduction parameter must be numeric, >= 0,",
          "and <= 1."))
  expect_error(freedom_design <- BayesianFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = NULL,
    pr_persist = 1,
    pr_freedom = 0.5,
    pr_intro = 0.1,
    iterations = NULL,
    confidence = 2),
    "The confidence of freedom parameter must be numeric, >= 0, and <= 1.")
  expect_silent(freedom_design <- BayesianFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = NULL,
    pr_persist = 1,
    pr_freedom = 0.5,
    pr_intro = 0.1,
    iterations = NULL,
    confidence = NULL))
  expect_is(freedom_design, "BayesianFreedomDesign")
  expect_s3_class(freedom_design, "AreaFreedomDesign")
  expect_null(freedom_design$get_evidence())
  expect_null(freedom_design$get_iterations())
})

test_that("calculates freedom evidence consistently with reference method", {
  TEST_DIRECTORY <- test_path("test_inputs")
  detected = as.logical(c(1, 0, 1, 0, 0, 1, 0, 0, 0)) # from Rout (2017)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Anderson2013_22_test.rds"))
  expect_silent(freedom_design <- BayesianFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = test_ref$calc_conf_growth,
    pr_persist = 1,
    pr_freedom = 0.5,
    iterations = 5,
    confidence = NULL))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_true(all(abs(round(as.numeric(evidence), 3) -
                        test_ref$calc_freedom_growth) <= 0.01))
  expect_equal(iterations, 5)
  expect_silent(freedom_design <- BayesianFreedomDesign( # discounted prior
    context = Context("test"),
    detected = FALSE,
    pr_detect = test_ref$calc_conf_growth, # 0.177,
    pr_persist = 1,
    pr_freedom = 0.5,
    pr_intro = 0.01,
    iterations = 5,
    confidence = NULL))
  expect_silent(evidence_disc <- freedom_design$get_evidence())
  expect_equal(evidence_disc[1], evidence[1])
  expect_true(all(evidence_disc[2:5] < evidence[2:5]))
  expect_evid <- 0.5/(1 - test_ref$calc_conf_growth[1]*(1 - 0.5))
  while (expect_evid[length(expect_evid)] < 0.95) {
    i <- length(expect_evid)
    j <- min(length(test_ref$calc_conf_growth), i + 1)
    expect_evid <- c(expect_evid,
                     expect_evid[i]/(1 - (test_ref$calc_conf_growth[j]*
                                            (1 - expect_evid[i]))))
  }
  expect_iter <- length(expect_evid)
  expect_silent(freedom_design <- BayesianFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = test_ref$calc_conf_growth,
    pr_persist = 1,
    pr_freedom = 0.5,
    iterations = NULL,
    confidence = 0.95))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), expect_evid)
  expect_equal(iterations, expect_iter)
  expect_silent(freedom_design <- BayesianFreedomDesign(
    context = Context("test"),
    detected = detected,
    pr_detect = c(rep(1, 6), test_ref$calc_conf_growth),
    pr_persist = 1,
    pr_freedom = 0.5,
    iterations = NULL,
    confidence = 0.95))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), c(rep(0, 6), expect_evid))
  expect_equal(iterations, expect_iter + 6)
  expect_evid <- rep(0, 6)
  for (i in 7:26) {
    B <- (3 - 1)/((i/6)^(3 - 1) - 1)
    expect_evid <- c(expect_evid,
                     1 - 1/(1 + (1 - 0.5)/0.5/B))
  }
  expect_silent(freedom_design <- BayesianFreedomDesign(
    context = Context("test"),
    detected = detected,
    pr_detect = NULL,
    pr_persist = 1,
    pr_freedom = 0.5,
    iterations = 26,
    confidence = NULL))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), expect_evid)
  expect_equal(iterations, 26)
  expect_equal(round(1 - evidence[c(9, 26)], 2),
               c(0.62, 0.10)) # Rout (2017, p337)
})
