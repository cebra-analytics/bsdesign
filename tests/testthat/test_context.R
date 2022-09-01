context("Context")

test_that("initializes with parameters", {
  expect_silent(conteX <- Context("My species"))
  expect_equal(conteX$get_species_name(), "My species")
  expect_equal(conteX$get_species_type(), "pest")
  expect_equal(conteX$get_surveillance_purpose(), "early_detection")
  expect_equal(conteX$get_surveillance_type(), "survey")
  expect_equal(conteX$get_surv_qty_unit(), "units")
  expect_equal(conteX$get_surv_cost_unit(), "$")
  expect_equal(conteX$get_mgmt_cost_unit(), "$")
  expect_equal(conteX$get_incursion_status(), "never_detected")
  expect_equal(conteX$get_area_freedom(), FALSE)
  expect_equal(conteX$get_market_access(), FALSE)
  expect_true(is.na(conteX$get_market_requirement()))
  expect_silent(conteX <- Context("My species", species_type = "weed"))
  expect_equal(conteX$get_species_type(), "weed")
  # User defined units
  expect_silent(conteX <- Context("My species",
                                  surv_qty_unit = "u1",
                                  surv_cost_unit = "u2",
                                  mgmt_cost_unit = "u3"))
  expect_equal(conteX$get_surv_qty_unit(), "u1")
  expect_equal(conteX$get_surv_cost_unit(), "u2")
  expect_equal(conteX$get_mgmt_cost_unit(), "u3")
})
