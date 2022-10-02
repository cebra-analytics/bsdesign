context("SpatialSurvDesign")

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  establish_pr <- test_ref$establish_pr
  expect_error(surv_design <- SpatialSurvDesign(context = Context("test"),
                                   divisions = divisions,
                                   establish_pr = establish_pr,
                                   lambda = 1:5,
                                   optimal = "none"),
               paste("The lambda parameter must be numeric, >= 0, and match",
                     "the number of division parts."))
  expect_error(surv_design <- SpatialSurvDesign(context = Context("test"),
                                   divisions = divisions,
                                   establish_pr = establish_pr,
                                   lambda = 1,
                                   prevalence = -1,
                                   optimal = "none"),
               "The prevalence parameter must be numeric and >= 0.")
  expect_error(surv_design <- SpatialSurvDesign(context = Context("test"),
                                   divisions = divisions,
                                   establish_pr = establish_pr,
                                   lambda = 1,
                                   optimal = "cost",
                                   mgmt_cost = list(a = 1, b = 2)),
               paste("The management cost parameter must contain list",
                     "elements 'detected' and 'undetected'."))
  expect_silent(surv_design <- SpatialSurvDesign(context = Context("test"),
                                                 divisions = divisions,
                                                 establish_pr = establish_pr,
                                                 lambda = 1,
                                                 optimal = "none"))
  expect_is(surv_design, "SpatialSurvDesign")
  expect_s3_class(surv_design, "SurveillanceDesign")
  expect_null(surv_design$get_allocation())
  expect_null(surv_design$get_sensitivity())
  expect_null(surv_design$get_confidence())
})

test_that("allocates resources consistently with reference method", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    budget = NULL))
  expect_silent(no_budget_alloc <- surv_design$get_allocation())
  expect_equal(round(no_budget_alloc, 8),
               round(test_ref$surv_effort$no_budget, 8))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    budget = test_ref$budget))
  expect_silent(with_budget_alloc <- surv_design$get_allocation())
  expect_equal(round(with_budget_alloc, 8),
               round(test_ref$surv_effort$with_budget, 8))
  expect_equal(sum(with_budget_alloc), test_ref$budget)
  expect_true(sum(with_budget_alloc) < sum(no_budget_alloc))
  expected_sensitivity <- 1 - exp(-1*test_ref$lambda*with_budget_alloc)
  expect_silent(sensitivity <- surv_design$get_sensitivity())
  expect_equal(sensitivity, expected_sensitivity)
  expected_confidence <- ((1 - prod(1 - test_ref$establish_pr*sensitivity))/
                            (1 - prod(1 - test_ref$establish_pr)))
  expect_silent(confidence <- surv_design$get_confidence())
  expect_equal(confidence, expected_confidence)
})

test_that("facilitates existing allocations and sensitivities", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  expected_sensitivity <- 1 - exp(-1*test_ref$lambda*
                                    test_ref$surv_effort$no_budget)
  exist_alloc <- c(test_ref$surv_effort$no_budget[1:198], rep(0, 199))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "none",
    mgmt_cost = list(),
    budget = NULL,
    exist_alloc = exist_alloc))
  expect_silent(exist_sens <- surv_design$get_sensitivity())
  expect_equal(exist_sens, c(expected_sensitivity[1:198], rep(0, 199)))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    budget = NULL,
    exist_sens = exist_sens))
  expect_silent(half_alloc <- surv_design$get_allocation())
  expect_equal(round(half_alloc, 8),
               round(c(rep(0, 198),
                       test_ref$surv_effort$no_budget[199:397]), 8))
  expect_silent(sensitivity <- surv_design$get_sensitivity())
  expect_equal(sensitivity, expected_sensitivity)
})
