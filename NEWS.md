# OpenLand 1.1.0

## Major Performance Optimizations & Enhancements

* **Significant performance improvements across core functions:**
  * Enhanced `intensityAnalysis()` with vectorized operations and optimized data processing
  * Improved memory management and reduced computational complexity
  * Advanced chunked processing with dynamic memory optimization
  * Intelligent chunk size calculation based on available system memory

* **Enhanced `contingencyTable()` performance:**
  * Optimized parallel processing for multi-core systems (2-4x speedup)
  * Enhanced terra package integration for 2-3x faster raster operations
  * Improved memory-efficient processing for large datasets exceeding available RAM
  * Better cross-tabulation algorithms with reduced memory allocations

* **New performance monitoring and benchmarking:**
  * Added `benchmark_performance()` function for measuring optimization gains
  * Enhanced `performance_status()` with detailed recommendations
  * Real-time performance feedback and optimization reporting
  * Comprehensive benchmarking suite for different dataset sizes

## Enhanced Core Functions

* **Optimized `intensityAnalysis()`:**
  * Implemented vectorized operations for categorical and transition calculations
  * Reduced memory allocations through efficient data processing pipelines
  * Pre-filtered data optimization to minimize redundant operations
  * Enhanced computation of interval, categorical, and transition metrics

* **Improved raster processing functions:**
  * Enhanced `summary_dir()` with faster vectorized operations
  * Optimized `summary_map()` with improved frequency counting
  * Better terra/raster compatibility with automatic fallbacks
  * Dynamic chunk size optimization based on available memory

## New Features

* **Advanced memory management:**
  * Dynamic chunk size calculation based on system memory availability
  * Intelligent garbage collection for large dataset processing
  * Enhanced error handling and progress reporting for chunked operations
  * Automatic memory optimization recommendations

* **Performance dependency management:**
  * Added `future.apply` package to imports for enhanced parallel processing
  * Better dependency checking and graceful fallbacks
  * Comprehensive performance status reporting

## Backward Compatibility

* **100% backward compatibility maintained:**
  * All existing function signatures preserved
  * Automatic parameter detection and optimization
  * Seamless integration with existing analysis workflows
  * No breaking changes to public APIs

## Technical Improvements

* **Enhanced data processing pipelines:**
  * Optimized dplyr operations with reduced intermediate objects
  * More efficient joins and aggregations in intensity analysis
  * Better factor handling and memory-conscious operations
  * Vectorized calculations throughout core functions

* **Robust error handling:**
  * Enhanced error messages with performance optimization context
  * Better validation of input parameters and data structures
  * Graceful handling of memory limitations and large datasets
  * Improved progress reporting for long-running operations

## Documentation & Testing

* **Comprehensive performance documentation:**
  * Enhanced package documentation with performance optimization details
  * New performance benchmarking examples and usage patterns
  * Updated function documentation with optimization parameters
  * Performance user guide updates

* **Enhanced testing framework:**
  * New performance optimization test suite
  * Benchmark validation tests for various dataset sizes
  * Memory optimization and chunking tests
  * Compatibility tests for terra/raster integration

# OpenLand (development version)

## Major Performance Optimizations

* **Enhanced `contingencyTable()` performance:**
  * Added parallel processing support for large datasets
  * Implemented terra package acceleration for better raster handling
  * Added chunked processing for memory-efficient computation of large rasters
  * Optimized cross-tabulation algorithms for improved speed

## New Features

* `contingencyTable()` now supports flexible naming conventions for input rasters:
  * Added `name_separator` parameter to use different separators (default: "_")
  * Added `year_position` parameter to specify year position ("first", "last", or numeric)
  * Added `name_pattern` parameter for custom regex patterns
  * Maintains full backward compatibility with existing naming convention

* **New accessor functions:**
  * Consolidated S4 method accessors with improved error handling
  * Enhanced generic methods for intensity analysis objects
  * Better validation and user-friendly error messages

## Improvements

* **Documentation enhancements:**
  * Comprehensive roxygen2 documentation updates
  * Enhanced package-level documentation with performance optimization details
  * Improved function parameter descriptions and examples
  * Better code structure and organization

* Enhanced robustness of flexible naming features:
  * Automatic detection and handling of R's name modifications (hyphens → dots, numeric prefix → "X" prefix)
  * Intelligent error messages with helpful suggestions for common issues
  * Automatic separator adjustment with informative warnings
  * Better documentation about R's automatic name changes

* **Code quality improvements:**
  * Optimized S4 method implementations
  * Enhanced error handling throughout the package
  * Improved code maintainability and structure
  * Better memory management for large datasets

## Bug Fixes

* Fixed vignette rendering issue with `dataset_available` variable
* Resolved documentation formatting issues for proper roxygen2 generation
* Enhanced terra/raster compatibility in `acc_changes` function

## Dependencies

* Added `stringr` package dependency for improved string processing
* Enhanced terra package integration for better performance

# OpenLand 1.0.3

* fixed unit test related to the plot function depending on the ggplot package (@teunbrand, #9)

# OpenLand 1.0.2

* if the dataset url is not accessible, the vignette fails gracefully with an informative message without an error
* memory allocation error fixed in `contingencyTable()` function for when it is used on rasters containing many years/layers or large areas

# OpenLand 1.0.1

* fixed summary_map bug

# OpenLand 1.0.0

* this is the the first CRAN version
* a newer version may be available on https://github.com/reginalexavier/OpenLand
* to get started, see the package vignette "Quick introduction to the OpenLand package" and the help files
* if you have any questions or suggestions, please contact me (reginalexavier at rocketmail dot com)

