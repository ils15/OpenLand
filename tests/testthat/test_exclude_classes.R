test_that("exclude_classes parameter works correctly", {
  
  # Skip if test data is not available
  skip_if_not_installed("raster")
  
  # Create simple test rasters with known values including class 0
  library(raster)
  
  # Create test rasters with values 0, 1, 2
  r1 <- raster(matrix(c(0, 0, 1, 1, 2, 2, 0, 1, 2), nrow = 3))
  r2 <- raster(matrix(c(0, 1, 1, 2, 2, 0, 1, 2, 0), nrow = 3))
  
  names(r1) <- "test_2020"
  names(r2) <- "test_2021"
  
  raster_stack <- raster::stack(r1, r2)
  
  # Test without exclusions
  result_no_exclude <- contingencyTable(raster_stack, pixelresolution = 1)
  
  # Test with class 0 excluded
  expect_message(
    result_exclude_0 <- contingencyTable(raster_stack, pixelresolution = 1, exclude_classes = 0),
    "Classes to be excluded from analysis: 0"
  )
  
  # Check that class 0 is not present in results
  expect_false(0 %in% result_exclude_0$lulc_Multistep$From)
  expect_false(0 %in% result_exclude_0$lulc_Multistep$To)
  expect_false(0 %in% result_exclude_0$tb_legend$categoryValue)
  
  # Check that excluded classes are stored as attribute
  expect_equal(attr(result_exclude_0$tb_legend, "excluded_classes"), 0L)
  
  # Test with multiple exclusions
  expect_message(
    result_exclude_multiple <- contingencyTable(raster_stack, pixelresolution = 1, exclude_classes = c(0, 2)),
    "Classes to be excluded from analysis: 0, 2"
  )
  
  # Check that both classes are excluded
  expect_false(any(c(0, 2) %in% result_exclude_multiple$lulc_Multistep$From))
  expect_false(any(c(0, 2) %in% result_exclude_multiple$lulc_Multistep$To))
  expect_false(any(c(0, 2) %in% result_exclude_multiple$tb_legend$categoryValue))
  
  # Test error handling
  expect_error(
    contingencyTable(raster_stack, exclude_classes = "invalid"),
    "exclude_classes must be a numeric vector"
  )
  
  expect_error(
    contingencyTable(raster_stack, exclude_classes = c(1, NA)),
    "exclude_classes cannot contain NA values"
  )
})

test_that("exclude_classes preserves functionality when NULL", {
  
  skip_if_not_installed("raster")
  
  # Create simple test rasters
  library(raster)
  
  r1 <- raster(matrix(c(1, 1, 2, 2, 3, 3, 1, 2, 3), nrow = 3))
  r2 <- raster(matrix(c(1, 2, 2, 3, 3, 1, 2, 3, 1), nrow = 3))
  
  names(r1) <- "test_2020"
  names(r2) <- "test_2021"
  
  raster_stack <- raster::stack(r1, r2)
  
  # Test with NULL exclude_classes (default behavior)
  result_default <- contingencyTable(raster_stack, pixelresolution = 1)
  result_explicit_null <- contingencyTable(raster_stack, pixelresolution = 1, exclude_classes = NULL)
  
  # Results should be identical
  expect_equal(result_default$lulc_Multistep, result_explicit_null$lulc_Multistep)
  expect_equal(result_default$tb_legend$categoryValue, result_explicit_null$tb_legend$categoryValue)
})
