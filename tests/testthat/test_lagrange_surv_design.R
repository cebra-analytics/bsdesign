context("LagrangeSurvDesign")

test_that("initializes with context, divisions, and valid parameters", {
  TEST_DIRECTORY <- test_path("test_inputs")
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = 10,
    establish_pr = NULL,
    f_obj = NULL,
    f_deriv = NULL,
    f_pos = NULL,
    alpha_unconstr = NULL,
    alpha_min = NULL,
    f_unit_sens = NULL,
    f_inv_unit_sens = NULL),
    paste("Divisions parameter must be a 'Divisions' or inherited class",
          "object."))
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:5,
    f_obj = NULL,
    f_deriv = NULL,
    f_pos = NULL,
    alpha_unconstr = NULL,
    alpha_min = NULL,
    f_unit_sens = NULL,
    f_inv_unit_sens = NULL),
    paste("The establishment probability must be numeric,  >= 0, and match",
          "the number of division parts."))
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = NULL,
    f_deriv = NULL,
    f_pos = NULL,
    alpha_unconstr = NULL,
    alpha_min = NULL,
    f_unit_sens = NULL,
    f_inv_unit_sens = NULL),
    "The unconstrained marginal benefit alpha value must be numeric.")
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = NULL,
    f_deriv = NULL,
    f_pos = NULL,
    alpha_unconstr = 0,
    alpha_min = NULL,
    f_unit_sens = NULL,
    f_inv_unit_sens = NULL),
    "The minimum marginal benefit alpha value must be numeric.")
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = NULL,
    f_deriv = NULL,
    f_pos = NULL,
    alpha_unconstr = 0,
    alpha_min = -1,
    f_unit_sens = NULL,
    f_inv_unit_sens = NULL),
    "The objective function should have form function(x_alloc).",
    fixed = TRUE)
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = function(x) 0,
    f_deriv = function() 0,
    f_pos = NULL,
    alpha_unconstr = 0,
    alpha_min = -1,
    f_unit_sens = NULL,
    f_inv_unit_sens = NULL),
    "The derivative function should have form function(x_alloc).",
    fixed = TRUE)
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = function(x) 0,
    f_deriv = function(x) 0,
    f_pos = function(a, b) 0,
    alpha_unconstr = 0,
    alpha_min = -1,
    f_unit_sens = NULL,
    f_inv_unit_sens = NULL),
    paste("The pseudo-inverse-derivative function should have form",
          "function(alpha)."), fixed = TRUE)
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = function(x) 0,
    f_deriv = function(x) 0,
    f_pos = function(a) 0,
    alpha_unconstr = 0,
    alpha_min = -1,
    f_unit_sens = NULL,
    f_inv_unit_sens = NULL),
    "The unit sensitivity function should have form function(x_alloc).",
    fixed = TRUE)
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = function(x) 0,
    f_deriv = function(x) 0,
    f_pos = function(a) 0,
    alpha_unconstr = 0,
    alpha_min = -1,
    f_unit_sens = function(x) 0,
    f_inv_unit_sens = NULL),
    paste("The inverse unit sensitivity function should have form",
          "function(unit_sens)."), fixed = TRUE)
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = function(x) 0,
    f_deriv = function(x) 0,
    f_pos = function(a) 0,
    alpha_unconstr = 0,
    alpha_min = -1,
    f_unit_sens = function(x) 0,
    f_inv_unit_sens = function(s) 0,
    budget = 0),
    "The budget parameter must be numeric and > 0.")
  expect_error(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = function(x) 0,
    f_deriv = function(x) 0,
    f_pos = function(a) 0,
    alpha_unconstr = 0,
    alpha_min = -1,
    f_unit_sens = function(x) 0,
    f_inv_unit_sens = function(s) 0,
    confidence = 2),
    "The detection confidence parameter must be numeric, >= 0 and <= 1.")
  expect_silent(surv_design <- LagrangeSurvDesign(
    context = Context("test"),
    divisions = Divisions(matrix(1:10)),
    establish_pr = 1:10,
    f_obj = function(x) 0,
    f_deriv = function(x) 0,
    f_pos = function(a) 0,
    alpha_unconstr = 0,
    alpha_min = -1,
    f_unit_sens = function(x) 0,
    f_inv_unit_sens = function(s) 0))
  expect_is(surv_design, "LagrangeSurvDesign")
  expect_equal(surv_design$get_cost_allocation(), 0)
})
