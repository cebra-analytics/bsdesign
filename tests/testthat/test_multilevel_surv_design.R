context("MultilevelSurvDesign")

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  divisions = Divisions(data.frame(level = 1:2))
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_sens = 2),
    "The sample sensitivity parameter must be a numeric value >= 0 and <= 1.")
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_sens = 1,
    sample_type = "discrete",
    prevalence = c(0.3, 0.4, 0.5)),
    paste("The prevalence parameter must be numeric with values >= 0 and",
          "<= 1 for each level."))
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_type = "continuous",
    prevalence = c(NA, 0.4, 0.5)),
    paste("The prevalence parameter must be numeric with values >= 0 and",
          "<= 1 for each level above the first."))
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_type = "discrete",
    prevalence = c(0.3, 0.4),
    total_indiv = 10),
    paste("The total individuals parameter must be numeric with values >= 0",
          "for each level."))
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_type = "continuous",
    prevalence =  c(NA, 0.4),
    total_indiv = 10),
    paste("The total individuals parameter must be numeric with values >= 0",
          "for each level above the first."))
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_type = "continuous",
    prevalence = c(NA, 0.4),
    total_indiv = c(NA, 10)),
    paste("The design density and sample area parameters are required for",
          "continuous sampling."))
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_type = "continuous",
    prevalence = c(NA, 0.4),
    total_indiv = c(NA, 10),
    design_dens = 0,
    sample_area = 0),
    "The design density parameter must be a numeric value > 0.")
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_type = "continuous",
    prevalence = c(NA, 0.4),
    total_indiv = c(NA, 10),
    design_dens = 1,
    sample_area = 0),
    "The sample area parameter must be a numeric value > 0.")
  expect_error(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_type = "continuous",
    prevalence = c(NA, 0.4),
    total_indiv = c(NA, 10),
    design_dens = 1,
    sample_area = 1,
    sample_cost = 2:4),
    paste("The sample cost parameter must be a numeric vector with  values",
          "for each level."))
  expect_silent(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_type = "discrete",
    prevalence = c(0.3, 0.4),
    total_indiv = c(5, 10),
    sample_cost = 2:3))
  expect_is(surv_design, "MultilevelSurvDesign")
  expect_s3_class(surv_design, "MultilevelSurvDesign")
})

test_that("allocates single stage sampling", {
  TEST_DIRECTORY <- test_path("test_inputs")
  divisions = Divisions(data.frame(level = 1))
  expect_silent(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_sens = 0.8,
    sample_type = "discrete",
    prevalence = 0.3,
    total_indiv = 10, # n/N > 0.1
    confidence = 0.9))
  expect_alloc <- ceiling(10/0.8*(1 - (1 - 0.9)^(1/0.3/10)))
  expect_sens <- 1 - (1 - 0.8*expect_alloc/10)^(0.3*10)
  expect_equal(surv_design$get_allocation(), expect_alloc)
  expect_equal(surv_design$get_sensitivity(), expect_sens)
  expect_equal(surv_design$get_confidence(), expect_sens)
  expect_silent(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_sens = 0.8,
    sample_type = "discrete",
    prevalence = 0.3,
    total_indiv = 100, # n/N < 0.1
    confidence = 0.9))
  expect_alloc <- ceiling(log(1 - 0.9)/log(1 - 0.8*0.3))
  expect_sens <- 1 - (1 - 0.8*0.3)^expect_alloc
  expect_equal(surv_design$get_allocation(), expect_alloc)
  expect_equal(surv_design$get_sensitivity(), expect_sens)
  expect_equal(surv_design$get_confidence(), expect_sens)
  expect_silent(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_sens = 0.8,
    sample_type = "continuous",
    design_dens = 2,
    sample_area = 0.04, # m^2
    confidence = 0.9))
  expect_alloc <- ceiling(-1*log(1 - 0.9)/0.8/2/0.04)
  expect_sens <- 1 - exp(-0.8*expect_alloc*0.04*2)
  expect_equal(surv_design$get_allocation(), expect_alloc)
  expect_equal(surv_design$get_sensitivity(), expect_sens)
  expect_equal(surv_design$get_confidence(), expect_sens)
})

test_that("allocates 2-stage sampling consistently with reference method", {
  TEST_DIRECTORY <- test_path("test_inputs")
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Kean2015_2stage_test.rds"))
  divisions <- Divisions(data.frame(level = test_ref$level,
                                    unit = test_ref$unit))
  expect_silent(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_sens = test_ref$sample_sens,
    sample_type = test_ref$sample_type,
    prevalence = test_ref$prevalence,
    total_indiv = test_ref$total_indiv,
    design_dens = test_ref$design_dens,
    sample_area = test_ref$sample_area,
    sample_cost = test_ref$sample_cost,
    confidence = test_ref$confidence))
  expect_sens <- 1 - exp(-1*test_ref$sample_sens*test_ref$sample_area*
                           test_ref$expected_n[1]*test_ref$design_dens)
  expect_sens <- c(expect_sens,
                   1 - ((1 - (expect_sens*test_ref$expected_n[2]/
                                test_ref$total_indiv[2]))
                        ^(test_ref$prevalence[2]*test_ref$total_indiv[2])))
  expect_equal(surv_design$get_allocation(), test_ref$expected_n)
  expect_equal(surv_design$get_sensitivity(), expect_sens)
  expect_equal(surv_design$get_confidence(), expect_sens[2])
})

test_that("allocates multistage sampling consistently with reference method", {
  TEST_DIRECTORY <- test_path("test_inputs")
  test_ref <- readRDS(file.path(TEST_DIRECTORY, "Kean2015_5stage_test.rds"))
  divisions <- Divisions(data.frame(level = test_ref$level,
                                    unit = test_ref$unit))
  expect_silent(surv_design <- MultilevelSurvDesign(
    context = Context("test"),
    divisions = divisions,
    sample_sens = test_ref$sample_sens,
    sample_type = test_ref$sample_type,
    prevalence = test_ref$prevalence,
    total_indiv = test_ref$total_indiv,
    design_dens = test_ref$design_dens,
    sample_area = test_ref$sample_area,
    sample_cost = test_ref$sample_cost,
    confidence = test_ref$confidence))
  expect_sens <- 1 - ((1 - test_ref$sample_sens*test_ref$prevalence[1])
                      ^test_ref$expected_n[1])
  for (i in 2:5) {
    expect_sens <- c(expect_sens,
                     1 - ((1 - (expect_sens[i - 1]*test_ref$expected_n[i]/
                                  test_ref$total_indiv[i]))
                          ^(test_ref$prevalence[i]*test_ref$total_indiv[i])))
  }
  expect_equal(surv_design$get_allocation(), test_ref$expected_n)
  expect_equal(surv_design$get_sensitivity(), expect_sens)
  expect_equal(surv_design$get_confidence(), expect_sens[5])
})
