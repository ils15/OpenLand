test_that("performance optimizations work correctly", {
  
  skip_if_not_installed("raster")
  
  # Test benchmark_performance function
  expect_silent(benchmark_result <- benchmark_performance(matrix_size = 50))
  
  # Check benchmark result structure
  expect_is(benchmark_result, "list")
  expect_true("terra_available" %in% names(benchmark_result))
  expect_true("future_available" %in% names(benchmark_result))
  expect_true("benchmarks" %in% names(benchmark_result))
  expect_true("recommendations" %in% names(benchmark_result))
  
  # Check that recommendations are provided
  expect_true(length(benchmark_result$recommendations) > 0)
  
  # Test performance_status function returns proper structure
  status_result <- performance_status()
  expect_is(status_result, "list")
  expect_true("terra_available" %in% names(status_result))
  expect_true("recommendations" %in% names(status_result))
  expect_true("performance_tips" %in% names(status_result))
})

test_that("enhanced chunked processing works", {
  
  skip_if_not_installed("terra")
  
  # Create small test SpatRasters
  library(terra)
  
  # Create test data
  m1 <- matrix(c(1, 1, 2, 2, 3, 3, 1, 2, 3), nrow = 3)
  m2 <- matrix(c(1, 2, 2, 3, 3, 1, 2, 3, 1), nrow = 3)
  
  r1 <- terra::rast(m1)
  r2 <- terra::rast(m2)
  
  # Test chunked processing
  expect_silent(result <- OpenLand:::.process_chunks(r1, r2, chunk_size = 5))
  
  # Check result structure
  expect_is(result, "data.frame")
  expect_true(all(c("From", "To", "count") %in% names(result)))
  expect_true(nrow(result) > 0)
  expect_true(all(result$count > 0))
})

test_that("optimized intensityAnalysis maintains compatibility", {
  
  skip_if_not_installed("raster")
  
  # Load built-in test data
  data(SL_2002_2014)
  
  # Test that intensityAnalysis still works with optimizations
  expect_silent(
    result <- intensityAnalysis(
      dataset = SL_2002_2014, 
      category_n = "Ap", 
      category_m = "SG", 
      area_km2 = TRUE
    )
  )
  
  # Check that result structure is maintained
  expect_is(result, "list")
  expect_equal(length(result), 6)  # Should have 6 elements as before
  
  # Check that key elements exist
  expect_true("lulc_table" %in% names(result))
  expect_true("lv1_tbl" %in% names(result))
  
  # Test with area_km2 = FALSE
  expect_silent(
    result_pixels <- intensityAnalysis(
      dataset = SL_2002_2014, 
      category_n = "Ap", 
      category_m = "SG", 
      area_km2 = FALSE
    )
  )
  
  expect_is(result_pixels, "list")
  expect_equal(length(result_pixels), 6)
})

test_that("summary functions use optimized operations", {
  
  skip_if_not_installed("raster")
  
  # Create test raster
  library(raster)
  
  test_matrix <- matrix(c(1, 1, 2, 2, 3, 3, 1, 2, 3), nrow = 3)
  test_raster <- raster::raster(test_matrix)
  names(test_raster) <- "test_2020"
  
  # Test summary_map with performance optimizations
  expect_silent(map_summary <- summary_map(test_raster))
  
  # Check result structure
  expect_is(map_summary, "data.frame")
  expect_true(all(c("pixvalue", "Qt") %in% names(map_summary)))
  expect_true(nrow(map_summary) > 0)
  
  # Test summary_dir with raster list
  test_raster2 <- test_raster
  names(test_raster2) <- "test_2021"
  raster_list <- list(test_raster, test_raster2)
  
  expect_silent(dir_summary <- summary_dir(raster_list))
  
  # Check result structure  
  expect_is(dir_summary, "data.frame")
  expect_true("file_name" %in% names(dir_summary))
  expect_equal(nrow(dir_summary), 2)  # Should have 2 rows for 2 rasters
})

test_that("performance optimizations handle edge cases", {
  
  # Test benchmark_performance with minimal data
  expect_silent(
    small_benchmark <- benchmark_performance(
      matrix_size = 10, 
      n_layers = 2, 
      run_full_benchmark = FALSE
    )
  )
  
  expect_is(small_benchmark, "list")
  
  # Test performance_status output
  expect_output(performance_status(), "OpenLand Performance Status")
  
  # Test that functions handle NULL inputs gracefully
  expect_error(summary_map(NULL))
  expect_error(summary_dir(NULL))
})