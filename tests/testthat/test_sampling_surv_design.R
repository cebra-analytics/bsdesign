context("SamplingSurvDesign")

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Cannon2009_C_test.rds"))
  divisions <- Divisions(as.matrix(test_ref$part))
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = c(0.1, 0.2),
    optimal = "none") ,
    paste("The sample sensitivity parameter must be numeric with values >= 0",
          "and <= 1 for each division part."))
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    prevalence = c(0.1, 0.2),
    optimal = "none") ,
    paste("The prevalence parameter must be numeric with values >= 0 and <= 1",
          "for each division part."))
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    total_indiv = 10,
    optimal = "none") ,
    paste("The total individuals parameter must be numeric with values > 0",
          "for each division part."))
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    design_dens = 1:2,
    optimal = "none") ,
    paste("The design density parameter must be numeric with values >= 0 for",
          "each division part."))
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_area = 0,
    optimal = "none") ,
    "The sample area parameter must be numeric with value > 0.")
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_cost = 1:2,
    optimal = "none") ,
    paste("The sample cost parameter must be a numeric vector with  values",
          "for each division part."))
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_type = "discrete",
    optimal = "none") ,
    "The prevalence parameter is required for discrete sampling.")
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_type = "continuous",
    optimal = "none") ,
    paste("The design density and sample area parameters are required for",
          "continuous sampling."))
  expect_error(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_type = "discrete",
    prevalence = 0.1,
    optimal = "cost",
    mgmt_cost = list(a = 1, b = 2)),
    paste("The management cost parameter must contain list elements",
          "'detected' and 'undetected'."))
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_type = "discrete",
    prevalence = 0.1,
    optimal = "none"))
  expect_is(surv_design, "SamplingSurvDesign")
  expect_s3_class(surv_design, "SurveillanceDesign")
  expect_null(surv_design$get_allocation())
  expect_null(surv_design$get_sensitivity())
  expect_null(surv_design$get_confidence())
})

test_that("allocates resources consistently with reference method", {
  TEST_DIRECTORY <- test_path("test_inputs")
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Cannon2009_C_test.rds"))
  divisions <- Divisions(as.matrix(test_ref$part))
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "detection",
    budget = test_ref$budget$ind_95))
  expect_equal(round(surv_design$get_allocation(), 4),
               round(test_ref$expected_n$detection, 4))
  expect_equal(round(surv_design$get_confidence(), 3),
               test_ref$confidence$ind_95)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "detection",
    confidence = test_ref$confidence$all))
  expect_equal(round(sum(surv_design$get_allocation())),
               test_ref$budget$all_95)
  expect_equal(round(surv_design$get_confidence(), 3), test_ref$confidence$all)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Cannon2009_D_test.rds"))
  divisions <- Divisions(as.matrix(test_ref$part))
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "benefit",
    benefit = test_ref$benefit,
    sample_cost = test_ref$sample_cost,
    budget = NULL))
  expect_equal(surv_design$get_allocation(), test_ref$expected_n$unrestricted)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "benefit",
    benefit = test_ref$benefit,
    sample_cost = test_ref$sample_cost,
    budget = test_ref$budget))
  expect_equal(surv_design$get_allocation(), test_ref$expected_n$restricted)
})
