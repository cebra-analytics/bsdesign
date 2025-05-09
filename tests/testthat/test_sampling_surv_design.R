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
  expect_null(surv_design$get_system_sens())
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
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = FALSE))
  expect_equal(round(surv_design$get_allocation(), 3),
               round(test_ref$expected_n$sensitivity, 3))
  expect_equal(round(surv_design$get_system_sens(), 3),
               test_ref$system_sens$ind_95)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "sensitivity",
    system_sens = test_ref$system_sens$all,
    discrete_alloc = FALSE))
  expect_equal(round(sum(surv_design$get_allocation())),
               test_ref$budget$all_95)
  expect_equal(round(surv_design$get_system_sens(), 3), test_ref$system_sens$all)
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
    budget = NULL,
    discrete_alloc = FALSE))
  expect_equal(round(surv_design$get_allocation(), 6),
               round(test_ref$expected_n$unrestricted, 6))
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
    budget = test_ref$budget,
    discrete_alloc = FALSE))
  expect_silent(saving_budget_alloc <- surv_design$get_allocation())
  expect_equal(round(saving_budget_alloc, 6),
               round(test_ref$expected_n$restricted, 6))
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
    budget = test_ref$budget,
    discrete_alloc = FALSE))
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
    budget = test_ref$budget,
    discrete_alloc = FALSE))
  expect_silent(benefit_budget_alloc <- surv_design$get_allocation())
  expect_equal(round(benefit_budget_alloc, 6),
               round(test_ref$expected_n$restricted, 6))
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
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = FALSE))
  expect_equal(round(surv_design$get_allocation(), 3),
               round(test_ref$expected_n$sensitivity, 3))
  total_indiv <- c(500, 200, 800, 600, 400) # n/N > 0.1
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    total_indiv = total_indiv,
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = FALSE))
  expect_silent(alloc <- surv_design$get_allocation())
  expect_true(all(round(alloc, 4) != round(test_ref$expected_n$sensitivity, 4)))
  expect_equal(round(sum(alloc)), test_ref$budget$ind_95)
  expect_equal(surv_design$get_sensitivity(),
               1 - (1 - 1*alloc/total_indiv)^(test_ref$prevalence*total_indiv))
})

test_that("facilitates existing allocations and sensitivities", {
  TEST_DIRECTORY <- test_path("test_inputs")
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Cannon2009_C_test.rds"))
  divisions <- Divisions(as.matrix(test_ref$part))
  exist_alloc <- test_ref$expected_n$sensitivity*c(1, 1, 1, 0, 0)
  establish_pr <- test_ref$establish_pr
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "none",
    exist_alloc = exist_alloc,
    discrete_alloc = FALSE))
  expect_null(surv_design$get_allocation())
  exist_sens <- 1 - (1 - 1*test_ref$prevalence)^exist_alloc
  expect_equal(surv_design$get_sensitivity(), exist_sens)
  expect_equal(surv_design$get_system_sens(),
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
    exist_sens = exist_sens,
    discrete_alloc = FALSE))
  expect_equal(surv_design$get_sensitivity(), exist_sens)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95 - sum(exist_alloc),
    exist_sens = exist_sens,
    discrete_alloc = FALSE))
  expect_equal(round(surv_design$get_allocation(), 3),
               round(test_ref$expected_n$sensitivity, 3)*c(0, 0, 0, 1, 1))
  expect_equal(round(surv_design$get_system_sens(), 3),
               test_ref$system_sens$ind_95)
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
    optimal = "sensitivity",
    fixed_cost = 10,
    budget = test_ref$budget$ind_95 + 50,
    discrete_alloc = FALSE))
  expect_equal(round(surv_design$get_allocation(), 3),
               round(test_ref$expected_n$sensitivity, 3))
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
    budget = NULL,
    discrete_alloc = FALSE))
  expect_silent(alloc <- surv_design$get_allocation())
  expect_equal(round(alloc, 8), round(test_ref$surv_effort$no_budget, 8))
})

test_that("allocates with minimum allocation", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  min_alloc <- c(rep(1, 200), rep(0, 197))
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
    budget = NULL,
    min_alloc = min_alloc,
    discrete_alloc = FALSE))
  expect_silent(min_alloc_no_budget <- surv_design$get_allocation())
  expect_equal(min_alloc_no_budget[1:200],
               1*(test_ref$establish_pr[1:200] > 0))
  expect_equal(round(min_alloc_no_budget[201:397], 6),
               round(test_ref$surv_effort$no_budget[201:397], 6))
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
    budget = test_ref$budget,
    min_alloc = 0.04,
    discrete_alloc = FALSE))
  expect_silent(min_alloc_with_budget <- surv_design$get_allocation())
  expect_equal(sum(min_alloc_with_budget), test_ref$budget)
  expect_equal(min(min_alloc_with_budget[which(min_alloc_with_budget > 0)]),
               0.04)
  below_idx <- which(test_ref$surv_effort$with_budget <= 0.04)
  expect_true(all(min_alloc_with_budget[below_idx] %in% c(0, 0.04)))
  above_idx <- which(test_ref$surv_effort$with_budget > 0.04)
  expect_true(all(min_alloc_with_budget[above_idx] >= 0.04))
})

test_that("allocates discrete integer allocations", {
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
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = TRUE))
  expect_true(all(abs(surv_design$get_allocation() -
                        test_ref$expected_n$sensitivity) < 1))
  expect_equal(sum(surv_design$get_allocation()), test_ref$budget$ind_95)
  expect_equal(round(surv_design$get_system_sens(), 3),
               test_ref$system_sens$ind_95)
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
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = TRUE))
  expect_silent(alloc1 <- surv_design$get_allocation())
  expect_true(all(abs(alloc1 - test_ref$expected_n$sensitivity) < 1))
  expect_equal(sum(alloc1), test_ref$budget$ind_95)
  total_indiv <- c(500, 200, 800, 600, 400) # n/N > 0.1
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    total_indiv = total_indiv,
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = TRUE))
  expect_silent(alloc2 <- surv_design$get_allocation())
  expect_true(sum(alloc1 != alloc2) > 3)
  expect_equal(sum(alloc2), test_ref$budget$ind_95)
  expect_equal(surv_design$get_sensitivity(),
               1 - ((1 - 1*alloc2/total_indiv)
                    ^(test_ref$prevalence*total_indiv)))
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  continuous_alloc <- test_ref$surv_effort$no_budget*100
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "continuous",
    design_dens = test_ref$lambda/100,
    sample_area = 1, # samples = area
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected*100,
                     detected = test_ref$cost_detected*100),
    discrete_alloc = TRUE))
  expect_silent(discrete_alloc <- surv_design$get_allocation())
  expect_true(all(discrete_alloc %in% 0:ceiling(max(continuous_alloc))))
  expect_true(all(discrete_alloc >= floor(continuous_alloc)))
  expect_true(all(discrete_alloc <= ceiling(continuous_alloc)))
  continuous_alloc <- test_ref$surv_effort$with_budget*100
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "continuous",
    design_dens = test_ref$lambda/100,
    sample_area = 1, # samples = area
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected*100,
                     detected = test_ref$cost_detected*100),
    budget = test_ref$budget*100,
    discrete_alloc = TRUE))
  expect_silent(discrete_alloc <- surv_design$get_allocation())
  expect_true(all(discrete_alloc %in% 0:ceiling(max(continuous_alloc))))
  expect_true(all(discrete_alloc >= floor(continuous_alloc)))
  expect_true(all(discrete_alloc <= ceiling(continuous_alloc)))
  expect_equal(sum(discrete_alloc), test_ref$budget*100)
})

test_that("handles establishment probabilities of one", {
  TEST_DIRECTORY <- test_path("test_inputs")
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Cannon2009_C_test.rds"))
  divisions <- Divisions(as.matrix(test_ref$part))
  establish_pr = test_ref$establish_pr/max(test_ref$establish_pr)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "detections",
    budget = test_ref$budget$ind_95,
    discrete_alloc = FALSE))
  expect_silent(alloc_detect_budget <- surv_design$get_allocation())
  expect_equal(round(sum(alloc_detect_budget), 6),
               round(test_ref$budget$ind_95, 6))
  expect_equal(round(alloc_detect_budget),
               round(test_ref$expected_n$sensitivity))
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "detections",
    system_sens = 0.99,
    discrete_alloc = FALSE))
  expect_silent(alloc_detect_sys <- surv_design$get_allocation())
  expect_equal(surv_design$get_system_sens(), 0.99)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = FALSE))
  expect_silent(alloc_sens_budget <- surv_design$get_allocation())
  expect_equal(round(sum(alloc_sens_budget), 6),
               round(test_ref$budget$ind_95, 6))
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    optimal = "sensitivity",
    system_sens = 0.99,
    discrete_alloc = FALSE))
  expect_silent(alloc_sens_sys <- surv_design$get_allocation())
  expect_equal(surv_design$get_system_sens(), 0.99)
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    total_indiv = c(5000, 2000, 8000, 6000, 4000), # n/N < 0.1
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = FALSE))
  expect_silent(alloc_sens_budget_lt_01 <- surv_design$get_allocation())
  expect_equal(round(sum(alloc_sens_budget_lt_01), 6),
               round(test_ref$budget$ind_95, 6))
  expect_silent(surv_design <- SamplingSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = test_ref$prevalence,
    total_indiv = c(500, 200, 800, 600, 400), # n/N > 0.1
    optimal = "sensitivity",
    budget = test_ref$budget$ind_95,
    discrete_alloc = FALSE))
  expect_silent(alloc_sens_budget_gt_01 <- surv_design$get_allocation())
  expect_equal(round(sum(alloc_sens_budget_gt_01), 6),
               round(test_ref$budget$ind_95, 6))
})
