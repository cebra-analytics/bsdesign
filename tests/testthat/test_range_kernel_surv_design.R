context("RangeKernelSurvDesign")

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  expect_error(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = Divisions(data.frame(1:2)),
    establish_pr = 0.01,
    lambda = 0,
    sigma = 0,
    intervals = 0,
    optimal = "none"),
    "Divisions must be spatial grid type.")
  expect_error(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = 0.01,
    lambda = 1:2,
    sigma = 0,
    intervals = 0,
    optimal = "none"),
    paste("The lambda parameter must be numeric, >= 0, and match the number",
          "of division parts."))
  expect_error(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = 0.01,
    lambda = 1,
    sigma = 0,
    intervals = 0,
    optimal = "none"),
    "The sigma parameter must be numeric and > 0.")
  expect_error(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = 0.01,
    lambda = 1,
    sigma = 1,
    intervals = 0,
    optimal = "none"),
    "The time intervals parameter must be numeric and > 0.")
  expect_error(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = 0.01,
    lambda = 1,
    sigma = 1,
    intervals = 1,
    prevalence = 0,
    optimal = "none"),
    "The prevalence parameter must be numeric and > 0.")
  expect_error(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = 0.01,
    lambda = 1,
    sigma = 1,
    intervals = 1,
    optimal = "none",
    exist_alloc = data.frame(a = 1)),
    paste("When the existing allocation parameter is a data frame (or",
          "matrix) it should contain coordinate data with columns named",
          "'lon' and 'lat'."), fixed = TRUE)
  expect_error(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = 0.01,
    lambda = 1,
    sigma = 1,
    intervals = 1,
    optimal = "none",
    exist_alloc = 1:2),
    paste("When the existing allocation parameter is a numeric vector it",
          "should have a value for each cell location."))
  expect_error(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = 0.01,
    lambda = 1,
    sigma = 1,
    intervals = 1,
    optimal = "none",
    exist_alloc = "invalid"),
    paste("The existing allocation parameter must be either a numeric vector",
          "or a data frame (or matrix) of coordinates."), fixed = TRUE)
  expect_silent(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = 0.01,
    lambda = 1,
    sigma = 1,
    intervals = 1,
    optimal = "none"))
  expect_is(surv_design, "RangeKernelSurvDesign")
  expect_s3_class(surv_design, "SurveillanceDesign")
  expect_null(surv_design$get_allocation())
  expect_null(surv_design$get_sensitivity())
  expect_null(surv_design$get_confidence())
})

test_that("allocates effectively with near optimal detection confidence", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Anderson2013_22_test.rds"))
  expect_silent(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    sigma = test_ref$sigma,
    intervals = test_ref$intervals,
    prevalence = test_ref$prevalence,
    optimal = "detection",
    budget = test_ref$budget))
  expect_silent(alloc_vect <- surv_design$get_allocation())
  expect_equal(sum(alloc_vect), test_ref$budget)
  expect_silent(alloc_coords <- surv_design$get_allocation(coords = TRUE))
  expect_named(alloc_coords, c("lon", "lat"))
  expect_equal(nrow(alloc_coords), test_ref$budget)
  expect_silent(sensitivity <- surv_design$get_sensitivity())
  expect_conf <- (sum(test_ref$establish_pr*sensitivity)/
                    sum(test_ref$establish_pr))
  expect_silent(confidence <- surv_design$get_confidence())
  expect_equal(confidence, expect_conf)
  rand_alloc_conf <- c()
  for (i in 1:10) {
    idx <- sample(divisions$get_parts(), 20)
    exist_alloc <- rep(0, divisions$get_parts())
    exist_alloc[idx] <- 1
    surv_design <- RangeKernelSurvDesign(
      context = Context("test"),
      divisions = divisions,
      establish_pr = test_ref$establish_pr,
      lambda = test_ref$lambda,
      sigma = test_ref$sigma,
      intervals = test_ref$intervals,
      prevalence = test_ref$prevalence,
      optimal = "none",
      exist_alloc = exist_alloc)
    rand_alloc_conf <- c(rand_alloc_conf, surv_design$get_confidence())
  }
  expect_true(confidence >= max(rand_alloc_conf))
})

test_that(paste("calculates sensitivity/confidence consistently with",
                "reference method"), {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Anderson2013_22_test.rds"))
  exist_alloc <- test_ref$alloc*1
  expect_silent(surv_design <- RangeKernelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    sigma = test_ref$sigma,
    intervals = test_ref$intervals,
    prevalence = test_ref$prevalence,
    optimal = "none",
    exist_alloc = exist_alloc))
  expect_silent(sensitivity <- surv_design$get_sensitivity())
  expect_conf <- (sum(test_ref$establish_pr*sensitivity)/
                    sum(test_ref$establish_pr))
  missing_idx <- which(test_ref$calc_sens == 0 & sensitivity > 0)
  expect_true((sum(round(sensitivity[-missing_idx], 3) ==
                     round(test_ref$calc_sens[-missing_idx], 3))/
                 (divisions$get_parts() - length(missing_idx))) > 0.95)
  expect_silent(confidence <- surv_design$get_confidence())
  expect_equal(confidence, expect_conf)
  expect_true(abs(test_ref$calc_conf - confidence)/test_ref$calc_conf < 0.05)
})
