# Test flexible naming conventions in contingencyTable

test_that("extract_year_from_name works with different separators", {
  # Test default underscore separator
  expect_equal(extract_year_from_name("landscape_2020"), "2020")
  
  # Test dash separator
  expect_equal(extract_year_from_name("landscape-2020", separator = "-"), "2020")
  
  # Test dot separator
  expect_equal(extract_year_from_name("landscape.2020", separator = "."), "2020")
})

test_that("extract_year_from_name works with different year positions", {
  # Test year at the end (default)
  expect_equal(extract_year_from_name("data_region_2020"), "2020")
  
  # Test year at the beginning
  expect_equal(extract_year_from_name("2020_data_region", position = "first"), "2020")
  
  # Test year at specific position
  expect_equal(extract_year_from_name("data_2020_region", position = 2), "2020")
})

test_that("extract_year_from_name works with custom patterns", {
  # Test simple pattern
  expect_equal(extract_year_from_name("landscape_data_2020_final", pattern = "\\d{4}"), "2020")
  
  # Test pattern with prefix
  expect_equal(extract_year_from_name("landscape_year2020_data", pattern = "year\\d{4}"), "year2020")
})

test_that("extract_year_from_name handles errors properly", {
  # Test missing separator
  expect_error(extract_year_from_name("landscape2020"), "does not contain separator")
  
  # Test invalid year format
  expect_error(extract_year_from_name("landscape_20"), "does not look like a 4-digit year")
  
  # Test position out of range
  expect_error(extract_year_from_name("landscape_2020", position = 5), "is out of range")
  
  # Test pattern not found
  expect_error(extract_year_from_name("landscape_2020", pattern = "xyz"), "Could not extract year")
})

test_that("contingencyTable accepts new parameters", {
  # This test would require actual raster data, so we'll just test that 
  # the function accepts the new parameters without error
  expect_error(
    contingencyTable(input_raster = "nonexistent", name_separator = "-"),
    "maps not found"  # Expected error for missing files, not parameter error
  )
  
  expect_error(
    contingencyTable(input_raster = "nonexistent", year_position = "first"),
    "maps not found"  # Expected error for missing files, not parameter error
  )
  
  expect_error(
    contingencyTable(input_raster = "nonexistent", name_pattern = "\\d{4}"),
    "maps not found"  # Expected error for missing files, not parameter error
  )
})
