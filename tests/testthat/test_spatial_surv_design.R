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
                                                mgmt_cost = list(a = 1,
                                                                 b = 2)),
               paste("The management cost parameter must contain list",
                     "elements 'detected' and 'undetected'."))
  expect_silent(surv_design <- SpatialSurvDesign(context = Context("test"),
                                                 divisions = divisions,
                                                 establish_pr = establish_pr,
                                                 lambda = 1,
                                                 optimal = "none"))
  expect_is(surv_design, "SpatialSurvDesign")
  expect_s3_class(surv_design, "SurveillanceDesign")
  expect_is(surv_design$get_context(), "Context")
  expect_is(surv_design$get_divisions(), "Divisions")
  expect_null(surv_design$get_allocation())
  expect_null(surv_design$get_sensitivity())
  expect_null(surv_design$get_system_sens())
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
    optimal = "saving",
    benefit = test_ref$cost_undetected - test_ref$cost_detected,
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
  expect_equal(round(with_budget_alloc, 6),
               round(test_ref$surv_effort$with_budget, 6))
  expect_equal(sum(with_budget_alloc), test_ref$budget)
  expect_true(sum(with_budget_alloc) < sum(no_budget_alloc))
  expected_sensitivity <- 1 - exp(-1*test_ref$lambda*with_budget_alloc)
  expect_silent(sensitivity <- surv_design$get_sensitivity())
  expect_equal(sensitivity, expected_sensitivity)
  expected_system_sens <- ((1 - prod(1 - test_ref$establish_pr*sensitivity))/
                            (1 - prod(1 - test_ref$establish_pr)))
  expect_silent(system_sens <- surv_design$get_system_sens())
  expect_equal(system_sens, expected_system_sens)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "saving",
    benefit = test_ref$cost_undetected - test_ref$cost_detected,
    budget = test_ref$budget))
  expect_silent(with_saving_alloc <- surv_design$get_allocation())
  expect_equal(round(with_saving_alloc, 6),
               round(test_ref$surv_effort$with_budget, 6))
  expect_equal(sum(with_saving_alloc), test_ref$budget)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "saving",
    benefit = (test_ref$cost_undetected - test_ref$cost_detected)/100,
    budget = test_ref$budget))
  expect_silent(with_alt_saving_alloc <- surv_design$get_allocation())
  non_zero <- which(with_alt_saving_alloc > 0)
  expect_equal(non_zero, which(test_ref$surv_effort$with_budget > 0))
  expect_true(all(with_alt_saving_alloc[non_zero] <
                    test_ref$surv_effort$with_budget[non_zero]))
  expect_true(sum(with_alt_saving_alloc) < test_ref$budget)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "benefit",
    benefit = (test_ref$cost_undetected - test_ref$cost_detected)/100,
    budget = test_ref$budget))
  expect_silent(with_benefit_alloc <- surv_design$get_allocation())
  expect_equal(round(with_benefit_alloc, 6),
               round(test_ref$surv_effort$with_budget, 6))
  expect_equal(sum(with_benefit_alloc), test_ref$budget)
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
    optimal = "none",
    exist_sens = exist_sens))
  expect_equal(surv_design$get_sensitivity(),
               c(expected_sensitivity[1:198], rep(0, 199)))
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
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    budget = NULL,
    system_sens = 0.9999,
    exist_sens = exist_sens))
  expect_silent(part_alloc <- surv_design$get_allocation())
  expect_true(all(part_alloc[1:198] == 0))
  expect_true(all(which(part_alloc > 0) %in% 199:divisions$get_parts()))
  expect_equal(surv_design$get_system_sens(), 0.9999)
})

test_that("allocates with fixed costs with and without budget", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  fixed_cost <- c(rep(1, 200), rep(0, 197))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected*10,
                     detected = test_ref$cost_detected*10),
    budget = test_ref$budget*10,
    alloc_cost = 10,
    fixed_cost = fixed_cost))
  expect_silent(budget_alloc <- surv_design$get_allocation())
  expect_true(all(budget_alloc <= test_ref$surv_effort$with_budget))
  expect_equal(sum(budget_alloc*10 + fixed_cost*(budget_alloc > 0)),
               test_ref$budget*10)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    budget = NULL,
    fixed_cost = fixed_cost*10))
  expect_silent(no_budget_alloc <- surv_design$get_allocation())
  mask <- (test_ref$surv_effort$no_budget + fixed_cost*10 <
             ((test_ref$cost_undetected - test_ref$cost_detected)*
                test_ref$establish_pr))
  expect_true(sum(mask) < divisions$get_parts())
  expect_equal(round(no_budget_alloc, 8),
               round(test_ref$surv_effort$no_budget*mask, 8))
})

test_that("allocates with minimum allocation", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  min_alloc <- c(rep(1, 200), rep(0, 197))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    budget = NULL,
    min_alloc = min_alloc))
  expect_silent(min_alloc_no_budget <- surv_design$get_allocation())
  expect_equal(min_alloc_no_budget[1:200],
               1*(test_ref$establish_pr[1:200] > 0))
  expect_equal(round(min_alloc_no_budget[201:397], 6),
               round(test_ref$surv_effort$no_budget[201:397], 6))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    budget = test_ref$budget,
    min_alloc = 0.04))
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
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  continuous_alloc <- test_ref$surv_effort$no_budget*100
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda/100,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected*100,
                     detected = test_ref$cost_detected*100),
    discrete_alloc = TRUE))
  expect_silent(discrete_alloc <- surv_design$get_allocation())
  expect_true(all(discrete_alloc %in% 0:ceiling(max(continuous_alloc))))
  expect_true(all(discrete_alloc >= floor(continuous_alloc)))
  expect_true(all(discrete_alloc <= ceiling(continuous_alloc)))
  continuous_alloc <- test_ref$surv_effort$with_budget*100
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda/100,
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
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda/100,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected*100,
                     detected = test_ref$cost_detected*100),
    system_sens = 0.999999,
    discrete_alloc = FALSE))
  expect_silent(continuous_alloc <- surv_design$get_allocation())
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda/100,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected*100,
                     detected = test_ref$cost_detected*100),
    system_sens = 0.999999,
    discrete_alloc = TRUE))
  expect_silent(discrete_alloc <- surv_design$get_allocation())
  expect_true(all(discrete_alloc %in% 0:ceiling(max(continuous_alloc))))
  expect_true(all(discrete_alloc >= floor(continuous_alloc)))
  expect_true(all(discrete_alloc <= ceiling(continuous_alloc)))
  expect_true(round(surv_design$get_system_sens(), 8) == 0.999999)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda/100,
    optimal = "sensitivity",
    system_sens = 0.999999,
    discrete_alloc = FALSE))
  expect_silent(continuous_alloc <- surv_design$get_allocation())
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda/100,
    optimal = "sensitivity",
    system_sens = 0.999999,
    discrete_alloc = TRUE))
  expect_silent(discrete_alloc <- surv_design$get_allocation())
  expect_true(all(discrete_alloc %in% 0:ceiling(max(continuous_alloc))))
  expect_true(sum(discrete_alloc >= floor(continuous_alloc)) >=
                divisions$get_parts() - 1)
  expect_true(all(discrete_alloc <= ceiling(continuous_alloc)))
  expect_true(round(surv_design$get_system_sens(), 8) == 0.999999)
})

test_that("allocates for optimal number of detections via system sens", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "benefit",
    benefit = 1,
    system_sens = 0.99))
  expect_silent(const_benefit_alloc <- surv_design$get_allocation())
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "detections",
    system_sens = 0.99))
  expect_silent(detections_alloc <- surv_design$get_allocation())
  expect_equal(detections_alloc, const_benefit_alloc)
  expect_equal(surv_design$get_system_sens(), 0.99)
})

test_that("allocates for optimal sensitivity via budget or system sens", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "none",
    exist_alloc = test_ref$surv_effort$with_budget))
  expect_silent(cost_budget_sens <- surv_design$get_sensitivity())
  expect_silent(cost_budget_sys_sens <- surv_design$get_system_sens())
  cost_budget_tot <- sum((test_ref$establish_pr*
                            (test_ref$cost_detected*cost_budget_sens +
                               (test_ref$cost_undetected*
                                  (1 - cost_budget_sens)))) +
                           test_ref$surv_effort$with_budget)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "sensitivity",
    budget = test_ref$budget))
  expect_silent(detect_budget_alloc <- surv_design$get_allocation())
  expect_silent(detect_budget_sens <- surv_design$get_sensitivity())
  expect_silent(detect_budget_sys_sens <- surv_design$get_system_sens())
  detect_budget_tot <- sum((test_ref$establish_pr*
                              (test_ref$cost_detected*detect_budget_sens +
                                 (test_ref$cost_undetected*
                                    (1 - detect_budget_sens)))) +
                             detect_budget_alloc)
  expect_equal(sum(detect_budget_alloc), test_ref$budget)
  expect_true(detect_budget_sys_sens > cost_budget_sys_sens)
  expect_true(detect_budget_tot > cost_budget_tot)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "cost",
    mgmt_cost = list(undetected = test_ref$cost_undetected,
                     detected = test_ref$cost_detected),
    system_sens = 0.99))
  expect_silent(cost_99_alloc <- surv_design$get_allocation())
  expect_silent(cost_99_sens <- surv_design$get_sensitivity())
  expect_silent(cost_99_sys_sens <- surv_design$get_system_sens())
  cost_99_tot <- sum((test_ref$establish_pr*
                        (test_ref$cost_detected*cost_99_sens +
                           (test_ref$cost_undetected*
                              (1 - cost_99_sens)))) +
                       cost_99_alloc)
  expect_equal(cost_99_sys_sens, 0.99)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = test_ref$establish_pr,
    lambda = test_ref$lambda,
    optimal = "sensitivity",
    system_sens = 0.99))
  expect_silent(detect_99_alloc <- surv_design$get_allocation())
  expect_silent(detect_99_sens <- surv_design$get_sensitivity())
  expect_silent(detect_99_sys_sens <- surv_design$get_system_sens())
  detect_99_tot <- sum((test_ref$establish_pr*
                          (test_ref$cost_detected*detect_99_sens +
                             (test_ref$cost_undetected*
                                (1 - detect_99_sens)))) +
                         detect_99_alloc)
  expect_equal(detect_99_sys_sens, 0.99)
  expect_true(sum(detect_99_alloc) < sum(cost_99_alloc))
  expect_true(detect_99_tot > cost_99_tot)
})

test_that("handles establishment probabilities of one", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Hauser2009_test.rds"))
  establish_pr <- test_ref$establish_pr/max(test_ref$establish_pr)
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    lambda = test_ref$lambda,
    optimal = "detections",
    budget = test_ref$budget))
  expect_silent(alloc_detect_budget <- surv_design$get_allocation())
  sum(alloc_detect_budget) # test_ref$budget
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    lambda = test_ref$lambda,
    optimal = "detections",
    system_sens = 0.99))
  expect_silent(alloc_detect_sys <- surv_design$get_allocation())
  surv_design$get_system_sens() # 0.99
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    lambda = test_ref$lambda,
    optimal = "sensitivity",
    budget = test_ref$budget))
  expect_silent(alloc_sens_budget <- surv_design$get_allocation())
  sum(alloc_sens_budget) # test_ref$budget
  expect_silent(surv_design <- SpatialSurvDesign(
    context = Context("test"),
    divisions = divisions,
    establish_pr = establish_pr,
    lambda = test_ref$lambda,
    optimal = "sensitivity",
    system_sens = 0.99))
  expect_silent(alloc_sens_sys <- surv_design$get_allocation())
  surv_design$get_system_sens() # 0.99
})
