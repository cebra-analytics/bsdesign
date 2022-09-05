context("Divisions")

test_that("initializes with planar CRS raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb.tif"))
  expect_silent(parts <- Divisions(template))
  expect_is(parts, "Divisions")
  expect_equal(parts$get_type(), "grid")
  expect_equal(parts$get_parts(), length(which(is.finite(template[]))))
  expect_true(parts$is_compatible(parts$get_template()))
  expect_equal(parts$get_indices(), which(is.finite(template[])))
  expect_equal(parts$get_res(), 1000)
  expect_equal(parts$is_included(5922:5925), c(TRUE, FALSE, FALSE, TRUE))
  expect_silent(features <- parts$get_feat())
  expect_is(features, "SpatVector")
  expect_length(features, parts$get_parts())
})

test_that("initializes with lonlat raster", {
  TEST_DIRECTORY <- test_path("test_inputs")
  template <- terra::rast(file.path(TEST_DIRECTORY, "greater_melb_wgs84.tif"))
  expect_silent(parts <- Divisions(template))
  coord <- terra::xyFromCell(template, 1113)
  mid_res <- terra::distance(coord, coord + 0.025, lonlat = TRUE)/sqrt(2)
  expect_true(abs(parts$get_res() - mid_res)/mid_res < 0.05)
  expect_equal(parts$is_included(1113:1116), c(TRUE, FALSE, FALSE, TRUE))
})

test_that("initializes with CSV data", {
  TEST_DIRECTORY <- test_path("test_inputs")
  locations <- utils::read.csv(file.path(TEST_DIRECTORY, "vic_cities.csv"))
  expect_silent(parts <- Divisions(locations))
  expect_equal(parts$get_type(), "patch")
  expect_equal(parts$get_parts(), nrow(locations))
  expect_true(parts$is_compatible(1:nrow(locations)))
  expect_equal(parts$get_data(), locations)
  expect_equal(parts$get_coords(), locations[, c("lon", "lat")])
  expect_equal(parts$get_coords(extra_cols = TRUE),
               locations[, c("lon", "lat", "name")])
  expect_silent(features <- parts$get_feat())
  expect_is(features, "SpatVector")
  expect_length(features, parts$get_parts())
  categories <- data.frame(categ = c("a", "b", "c"), other = 1:3)
  expect_silent(parts <- Divisions(categories))
  expect_equal(parts$get_type(), "other")
})