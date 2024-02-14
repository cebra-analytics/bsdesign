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
  expect_is(surv_design$get_context(), "Context")
  expect_is(surv_design$get_divisions(), "Divisions")
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
  expect_equal(round(surv_design$get_allocation(), 3),
               round(test_ref$expected_n$detection, 3))
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
    optimal = "saving",
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
    optimal = "saving",
    benefit = test_ref$benefit,
    sample_cost = test_ref$sample_cost,
    budget = test_ref$budget))
  expect_silent(saving_budget_alloc <- surv_design$get_allocation())
  saving_budget_alloc # test_ref$expected_n$restricted
  expect_equal(saving_budget_alloc, test_ref$expected_n$restricted)
  expect_equal(sum(saving_budget_alloc*test_ref$sample_cost), test_ref$budget)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "saving",
    benefit = test_ref$benefit/4,
    sample_cost = test_ref$sample_cost,
    budget = test_ref$budget))
  expect_silent(saving_budget_alloc <- surv_design$get_allocation())
  expect_true(all(saving_budget_alloc < test_ref$expected_n$restricted))
  expect_true(sum(saving_budget_alloc*test_ref$sample_cost) < test_ref$budget)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "benefit",
    benefit = test_ref$benefit/4,
    sample_cost = test_ref$sample_cost,
    budget = test_ref$budget))
  expect_silent(benefit_budget_alloc <- surv_design$get_allocation())
  expect_equal(benefit_budget_alloc, test_ref$expected_n$restricted)
  expect_equal(sum(benefit_budget_alloc*test_ref$sample_cost), test_ref$budget)
})

test_that("allocates when sample fraction n/N <= 0.1 and > 0.1", {
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
    total_indiv = c(5000, 2000, 8000, 6000, 4000), # n/N < 0.1
    optimal = "detection",
    budget = test_ref$budget$ind_95))
  expect_equal(round(surv_design$get_allocation(), 3),
               round(test_ref$expected_n$detection, 3))
  total_indiv <- c(500, 200, 800, 600, 400) # n/N > 0.1
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    total_indiv = total_indiv,
    optimal = "detection",
    budget = test_ref$budget$ind_95))
  expect_silent(alloc <- surv_design$get_allocation())
  expect_true(all(round(alloc, 4) != round(test_ref$expected_n$detection, 4)))
  expect_equal(round(sum(alloc)), test_ref$budget$ind_95)
  expect_equal(surv_design$get_sensitivity(),
               1 - (1 - 1*alloc/total_indiv)^(test_ref$prevalence*total_indiv))
})

test_that("facilitates existing allocations and sensitivities", {
  TEST_DIRECTORY <- test_path("test_inputs")
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Cannon2009_C_test.rds"))
  divisions <- Divisions(as.matrix(test_ref$part))
  exist_alloc <- test_ref$expected_n$detection*c(1, 1, 1, 0, 0)
  establish_pr <- test_ref$establish_pr
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "none",
    exist_alloc = exist_alloc))
  expect_null(surv_design$get_allocation())
  exist_sens <- 1 - (1 - 1*test_ref$prevalence)^exist_alloc
  expect_equal(surv_design$get_sensitivity(), exist_sens)
  expect_equal(surv_design$get_confidence(),
               ((1 - prod(1 - establish_pr*exist_sens))/
                  (1 - prod(1 - establish_pr))))
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "none",
    exist_sens = exist_sens))
  expect_equal(surv_design$get_sensitivity(), exist_sens)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "detection",
    budget = test_ref$budget$ind_95 - sum(exist_alloc),
    exist_sens = exist_sens))
  expect_equal(round(surv_design$get_allocation(), 3),
               round(test_ref$expected_n$detection, 3)*c(0, 0, 0, 1, 1))
  expect_equal(round(surv_design$get_confidence(), 3),
               test_ref$confidence$ind_95)
})

test_that("allocates budget with fixed costs", {
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
    fixed_cost = 10,
    budget = test_ref$budget$ind_95 + 50))
  expect_equal(round(surv_design$get_allocation(), 3),
               round(test_ref$expected_n$detection, 3))
})

test_that("allocates continuous sampling consistently with reference method", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "continuous",
    design_dens = test_ref$lambda,
    sample_area = 1, # samples = area
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    budget = NULL))
  expect_silent(alloc <- surv_design$get_allocation())
  expect_equal(round(alloc, 8), round(test_ref$surv_effort$no_budget, 8))
})
