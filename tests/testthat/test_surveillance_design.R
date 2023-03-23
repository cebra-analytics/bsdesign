context("SurveillanceDesign")

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = "invalid"),
               paste("Divisions parameter must be a 'Divisions' or inherited",
                     "class object."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    establish_pr = 1:5),
               paste("The establishment probability must be a numeric vector",
                     "with values >= 0 for each division part."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    mgmt_cost = list(a = 1:5)),
               paste("The management cost parameter must be a list of numeric",
                     "vectors with values for each division part."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    benefit = 1:5),
               paste("The benefit parameter must be a numeric vector with",
                     "values for each division part."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    confidence = 1.5),
               paste("The detection confidence parameter must be numeric,",
                     ">= 0 and <= 1."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "cost",
                                    mgmt_cost = list()),
               paste("The management cost parameter must be specified for",
                     "optimal cost."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "benefit",
                                    benefit = NULL),
               "The benefit parameter must be specified for optimal benefit.")
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "detection",
                                    budget = NULL,
                                    confidence = NULL),
               paste("Either the budget or detection confidence parameter",
                     "must be specified for optimal detection."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "cost",
                                    mgmt_cost = list(a = 1),
                                    alloc_cost = 1:5),
               paste("The allocation cost parameter must be a numeric vector",
                     "with values for each division part."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "cost",
                                    mgmt_cost = list(a = 1),
                                    fixed_cost = 1:5),
               paste("The fixed cost parameter must be a numeric vector with",
                     "values for each division part."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "detection",
                                    budget = 0),
               "The budget parameter must be numeric and > 0.")
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                                 divisions = divisions,
                                                 optimal = "detection",
                                                 budget = 1,
                                                 min_alloc = 1:5),
               paste("The minimum allocation parameter must be a numeric",
                     "vector with values for each division part."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                                 divisions = divisions,
                                                 optimal = "detection",
                                                 budget = 1,
                                                 discrete_alloc = NULL),
               "The discrete allocation indicator parameter must be logical.")
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "detection",
                                    budget = 1,
                                    exist_alloc = 1),
               paste("The existing allocation parameter should only be",
                     "specified when the optimal parameter is 'none'."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "none",
                                    exist_alloc = 1),
               paste("The existing allocation parameter must be a numeric",
                     "vector with values for each division part."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "none",
                                    exist_sens = 1:5),
               paste("The existing sensitivity parameter must be a numeric",
                     "vector, or list of numeric vectors, with values for",
                     "each division part."))
  expect_error(surv_design <- SurveillanceDesign(context = Context("test"),
                                    divisions = divisions,
                                    optimal = "none",
                                    exist_sens = list(a = 1, b = 1:5)),
               paste("The existing sensitivity parameter must be a numeric",
                     "vector, or list of numeric vectors, with values for",
                     "each division part."))
  expect_silent(surv_design <- SurveillanceDesign(context = Context("test"),
                                                  divisions = divisions,
                                                  optimal = "none"))
  expect_is(surv_design, "SurveillanceDesign")
  expect_null(surv_design$get_allocation())
  expect_null(surv_design$get_sensitivity())
  expect_null(surv_design$get_confidence())
})

test_that("combines existing sensitivities via union", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "template.tif"))
  divisions <- Divisions(template)
  set.seed(1234)
  exist_sens <- lapply(1:3, function(i) runif(divisions$get_parts(), 0, 1))
  expect_silent(surv_design <- SurveillanceDesign(context = Context("test"),
                                                  divisions = divisions,
                                                  optimal = "none",
                                                  exist_sens = exist_sens))
  expected_sensitivity <-
    1 - apply(cbind(exist_sens[[1]], exist_sens[[2]], exist_sens[[3]]), 1,
              function(s) prod(1 - s))
  expect_equal(round(surv_design$get_sensitivity(), 8),
               round(expected_sensitivity, 8))
  expect_null(surv_design$get_confidence())
})
