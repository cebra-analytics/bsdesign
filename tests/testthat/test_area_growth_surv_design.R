context("AreaGrowthSurvDesign")

LIMITED <- TRUE #FALSE

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  divisions <- Divisions(data.frame(subregions = 1:3))
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500),
    establish_rate = c(0.01, 0.02),
    growth_rate = c(1.1, 1.2),
    mgmt_cost = list(a = 3:5, b = 5)),
    paste("The sub-region area parameter must be a numeric vector with values",
          "> 0 for each sub-region."))
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02),
    growth_rate = c(1.1, 1.2),
    mgmt_cost = list(a = 3:5, b = 5)),
    paste("The establishment rate must be a numeric vector with values >= 0",
          "for each sub-region."))
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02, 0.03),
    growth_rate = c(1.1, 1.2),
    mgmt_cost = list(a = 3:5, b = 5)),
    paste("The growth rate parameter must be a numeric vector with values",
          ">= 0 for each sub-region."))
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02, 0.03),
    growth_rate = c(1.1, 1.2, 1.3),
    mgmt_cost = list(a = 3:5, b = 5)),
    paste("The management cost parameter must contain list elements",
          "'eradication', 'damage', and 'penalty'."))
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02, 0.03),
    growth_rate = c(1.1, 1.2, 1.3),
    size_class_max = 0,
    mgmt_cost = list(eradication = 3:5, damage = 5, penalty = 20)),
    "The maximum size class parameter must be numeric and > 0.")
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02, 0.03),
    growth_rate = c(1.1, 1.2, 1.3),
    class_pops_max = 0,
    mgmt_cost = list(eradication = 3:5, damage = 5, penalty = 20)),
    paste("The maximum number of populations in a size class parameter must",
          "be numeric and > 0."))
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02, 0.03),
    growth_rate = c(1.1, 1.2, 1.3),
    f_area_growth = function(g) 0, # g, s
    mgmt_cost = list(eradication = 3:5, damage = 5, penalty = 20)),
    paste("The area growth function should have form function(growth_rate,",
          "size_class)."), fixed = TRUE)
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02, 0.03),
    growth_rate = c(1.1, 1.2, 1.3),
    f_area_growth = function(g, s) 0, # g, s
    sample_sens = 2,
    mgmt_cost = list(eradication = 3:5, damage = 5, penalty = 20)),
    "The sample sensitivity parameter must be a numeric value >= 0 and <= 1.")
  expect_error(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02, 0.03),
    growth_rate = c(1.1, 1.2, 1.3),
    mgmt_cost = list(eradication = 3:5, damage = 5, penalty = 20),
    sample_cost = 7:8),
    paste("The sample cost parameter must be a numeric vector with  values",
          "for each sub-region."))
  expect_silent(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = c(400, 500, 600),
    establish_rate = c(0.01, 0.02, 0.03),
    growth_rate = c(1.1, 1.2, 1.3),
    mgmt_cost = list(eradication = 3:5, damage = 5, penalty = 20)))
  expect_is(surv_design, "AreaGrowthSurvDesign")
  expect_s3_class(surv_design, "SurveillanceDesign")
})

test_that("allocates resources consistently with reference method", {
  TEST_DIRECTORY <- test_path("test_inputs")
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "EpanchinNiell2012_test.rds"))
  if (LIMITED) {
    SR <- 5
  } else {
    SR <- length(test_ref$table_S1_data$subregions)
  }
  divisions <- Divisions(data.frame(
    subregions = test_ref$table_S1_data$subregions[1:SR]))
  subregion_area <- test_ref$table_S1_data$subregion_area[1:SR]
  establish_rate <- test_ref$table_S1_data$establish_rate[1:SR]
  growth_rate <- test_ref$table_1_data$growth_rate
  size_class_max <- test_ref$table_1_data$size_class_max
  class_pops_max <- test_ref$table_1_data$class_pops_max
  f_area_growth <- test_ref$table_1_data$f_area_growth
  sample_sens <- test_ref$table_1_data$sample_sens
  mgmt_cost <- test_ref$table_1_data$mgmt_cost
  sample_cost <- test_ref$table_S1_data$sample_cost[1:SR]
  expect_costs <- test_ref$table_S2_data[c("sum_survey_cost",
                                           "sum_total_cost")]
  expect_silent(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = subregion_area,
    establish_rate = establish_rate,
    growth_rate = growth_rate,
    size_class_max = size_class_max,
    class_pops_max = class_pops_max,
    f_area_growth = f_area_growth,
    sample_sens = sample_sens,
    mgmt_cost = mgmt_cost,
    sample_cost = sample_cost,
    budget = NULL))
  if (!LIMITED) {
    surv_design$set_cores(8)
  }
  surv_design$set_precision(2)
  expect_silent(alloc <- surv_design$get_allocation())
  expect_true(max(abs(
    round(as.numeric(alloc), 3) -
      test_ref$table_S1_data$expected_density[1:SR])) <= 0.01)
  expect_sens <- 1 - exp(-1*sample_sens*subregion_area*as.numeric(alloc))
  expect_equal(round(surv_design$get_sensitivity(), 8), round(expect_sens, 8))
  expect_equal(surv_design$get_confidence(),
               sum(establish_rate*expect_sens)/sum(establish_rate))
  if (!LIMITED) {
    expect_true((abs(sum(attr(alloc, "costs")$survey_cost) -
                       expect_costs$sum_survey_cost$no_budget)/
                   expect_costs$sum_survey_cost$no_budget) <= 0.001)
    expect_true((abs(sum(attr(alloc, "costs")$total_cost) -
                       expect_costs$sum_total_cost$no_budget)/
                   expect_costs$sum_total_cost$no_budget) <= 0.001)
  }
  expect_silent(surv_design <- AreaGrowthSurvDesign(
    context = Context("test"),
    divisions = divisions,
    subregion_area = subregion_area,
    establish_rate = establish_rate,
    growth_rate = growth_rate,
    size_class_max = size_class_max,
    class_pops_max = class_pops_max,
    f_area_growth = f_area_growth,
    sample_sens = sample_sens,
    mgmt_cost = mgmt_cost,
    sample_cost = sample_cost,
    budget = sum(test_ref$table_S1_data$expected_survey_cost[1:SR])/2))
  if (!LIMITED) {
    surv_design$set_cores(8)
  }
  surv_design$set_precision(2)
  expect_silent(alloc <- surv_design$get_allocation())
  expect_equal(round(mean(
    as.numeric(alloc)/test_ref$table_S1_data$expected_density[1:SR]), 2), 0.5)
  if (!LIMITED) {
    expect_equal(sum(sum(attr(alloc, "costs")$survey_cost)),
                 expect_costs$sum_survey_cost$budget_50)
    expect_true((abs(sum(attr(alloc, "costs")$total_cost) -
                       expect_costs$sum_total_cost$budget_50)/
                   expect_costs$sum_total_cost$budget_50) <= 0.001)
  }
})
