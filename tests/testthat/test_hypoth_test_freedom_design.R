context("HypothTestFreedomDesign")

test_that("initializes with context and valid parameters", {
  expect_error(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = NULL,
    pr_persist = 1,
    iterations = NULL,
    p_value = -1),
    "The hypothesis test p-value parameter must be numeric, >= 0, and <= 1.")
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = NULL,
    pr_persist = 1,
    iterations = NULL,
    p_value = NULL))
  expect_is(freedom_design, "HypothTestFreedomDesign")
  expect_s3_class(freedom_design, "AreaFreedomDesign")
  expect_null(freedom_design$get_evidence())
  expect_null(freedom_design$get_iterations())
})

test_that("calculates freedom evidence consistently with reference method", {
  detected = as.logical(c(1, 0, 1, 0, 0, 1, 0, 0, 0)) # from Rout (2017)
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = detected,
    pr_detect = NULL,
    pr_persist = 1,
    iterations = NULL,
    p_value = NULL))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), c(rep(1, 6), (6/(7:9))^3))
  expect_equal(iterations, length(detected))
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = detected,
    pr_detect = NULL,
    pr_persist = 1,
    iterations = 20,
    p_value = NULL))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), c(rep(1, 6), (6/(7:20))^3))
  expect_equal(iterations, 20)
  expect_iter <- 20
  expect_evid <-evidence[1:expect_iter]
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = detected,
    pr_detect = NULL,
    pr_persist = 1,
    iterations = 20,
    p_value = 0.01))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), expect_evid)
  expect_equal(iterations, expect_iter)
  expect_true(all(evidence > 0.01))
  expect_iter <- min(which(evidence <= 0.04))
  expect_evid <- evidence[1:expect_iter]
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = detected,
    pr_detect = NULL,
    pr_persist = 1,
    iterations = 20,
    p_value = 0.04))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), expect_evid)
  expect_equal(iterations, expect_iter)
  expect_equal(sum(evidence > 0.04), iterations - 1)
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = detected,
    pr_detect = NULL,
    pr_persist = 1,
    iterations = NULL,
    p_value = 0.01))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), c(rep(1, 6), (6/(7:iterations))^3))
  expect_equal(sum(evidence > 0.01), iterations - 1)
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = 0.3,
    pr_persist = 0.9,
    iterations = 12,
    p_value = NULL))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), (0.9*(1 - 0.3))^(1:12))
  expect_iter <- min(which(evidence <= 0.02))
  expect_evid <- evidence[1:expect_iter]
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = 0.3,
    pr_persist = 0.9,
    iterations = 12,
    p_value = 0.02))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), expect_evid)
  expect_equal(iterations, expect_iter)
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = 0.3,
    pr_persist = 0.9,
    iterations = NULL,
    p_value = 0.005))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), (0.9*(1 - 0.3))^(1:iterations))
  expect_equal(sum(evidence > 0.005), iterations - 1)
  expect_iter <- iterations + 6
  expect_evid <- c(rep(1, 6), evidence)
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = detected,
    pr_detect = 0.3,
    pr_persist = 0.9,
    iterations = NULL,
    p_value = 0.005))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), expect_evid)
  expect_equal(iterations, expect_iter)
  pr_detect <- 0.3 + (0:11)/100
  pr_persist <- 0.9 - (0:11)/100
  expect_evid <- 0.9*(1 - 0.3)
  for (i in 2:12) {
    expect_evid <- c(expect_evid,
                     expect_evid[i - 1]*pr_persist[i]*(1 - pr_detect[i]))
  }
  expect_silent(freedom_design <- HypothTestFreedomDesign(
    context = Context("test"),
    detected = FALSE,
    pr_detect = pr_detect,
    pr_persist = pr_persist,
    iterations = 12,
    p_value = NULL))
  expect_silent(evidence <- freedom_design$get_evidence())
  expect_silent(iterations <- freedom_design$get_iterations())
  expect_equal(as.numeric(evidence), expect_evid)
})
