
#' @importFrom dplyr bind_rows tibble
#' @importFrom terra rast ext res nrow ncol minmax crs freq nlyr
NULL

#' Summary of multiple parameters in a raster directory
#'
#' Listing major characteristics of raster inputs. Those characteristics are the
#' dimensions, the resolution, the extent, the values (min, max) and the
#' coordinate reference system. This function supports both raster and terra objects
#' for improved performance.
#'
#' @param path The path for the Raster*/SpatRaster directory or list of Raster*/SpatRaster to be
#' analysed. Supports raster package objects and terra package objects.
#'
#' @return Table with the raster parameters in columns
#' @export
#'
#' @examples
#' \donttest{
#' url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
#' temp <- tempfile()
#' download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
#' load(temp)
#' # the summary_dir() function, with the SaoLourencoBasin dataset
#'
#' summary_dir(raster::unstack(SaoLourencoBasin))
#' }
#'
summary_dir <- function(path) {
  
  # Handle different input types with terra/raster compatibility
  if (inherits(path, "list")) {
    # Check if it's a list of raster/terra objects
    if (inherits(path[[1]], c("RasterLayer", "SpatRaster"))) {
      layer_list <- path
    } else {
      stop("List elements must be RasterLayer or SpatRaster objects")
    }
  } else if (inherits(path, "character")) {
    # Directory path - load files vectorized
    raster_files <- list.files(path, pattern = ".tif$", full.names = TRUE)
    
    if (length(raster_files) == 0) {
      stop("No .tif files found in the specified directory")
    }
    
    # Use terra for faster loading - vectorized approach
    # terra::rast can handle multiple files at once
    tryCatch({
      # Try terra first (faster)
      layer_stack <- terra::rast(raster_files)
      layer_list <- as.list(layer_stack)
    }, error = function(e) {
      # Fallback to raster if terra fails
      warning("Terra loading failed, falling back to raster package")
      layer_list <- lapply(raster_files, raster::raster)
    })
  } else if (inherits(path, c("RasterStack", "RasterBrick", "SpatRaster"))) {
    # Handle stack/brick objects
    if (inherits(path, "SpatRaster")) {
      layer_list <- as.list(path)
    } else {
      layer_list <- raster::unstack(path)
    }
  } else {
    stop("Input must be a character path, list of raster objects, or raster stack/brick")
  }
  
  # Vectorized summary extraction using appropriate package functions
  summary_list <- lapply(layer_list, function(x) {
    
    # Determine which package functions to use based on object type
    if (inherits(x, "SpatRaster")) {
      # Use terra functions for SpatRaster
      ext_obj <- terra::ext(x)
      res_obj <- terra::res(x)
      tibble::tibble(
        file_name = names(x),
        xmin = ext_obj[1],
        xmax = ext_obj[2],
        ymin = ext_obj[3],
        ymax = ext_obj[4],
        res_x = res_obj[1],
        res_y = res_obj[2],
        nrow = terra::nrow(x),
        ncol = terra::ncol(x),
        min_val = terra::minmax(x)[1],
        max_val = terra::minmax(x)[2],
        crs = as.character(terra::crs(x))
      )
    } else {
      # Use raster functions for RasterLayer
      tibble::tibble(
        file_name = base::names(x),
        xmin = raster::xmin(x),
        xmax = raster::xmax(x),
        ymin = raster::ymin(x),
        ymax = raster::ymax(x),
        res_x = raster::res(x)[1],
        res_y = raster::res(x)[2],
        nrow = raster::nrow(x),
        ncol = raster::ncol(x),
        min_val = raster::minValue(x),
        max_val = raster::maxValue(x),
        crs = as.character(raster::crs(x))
      )
    }
  })
  
  # Efficient binding using dplyr
  dplyr::bind_rows(summary_list)
}


#' Quantitative summary of a unique categorical raster
#'
#' This function presents a summary with the pixel quantity of each category
#' present in a categorical raster. Supports both raster and terra objects
#' for improved performance.
#'
#' @param path The path for the raster to be analysed, if path is a multilayer
#' raster only the first RasterLayer/SpatRaster will be analysed. Supports both
#' raster package objects and terra package objects.
#'
#' @return A table containing in columns the pixel counts for each pixel value
#'
#' @export
#'
#' @examples
#' \donttest{
#' url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
#' temp <- tempfile()
#' download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
#' load(temp)
#' summary_map(SaoLourencoBasin[[1]])
#' }
#'
summary_map <- function(path) {
  
  # Handle different input types with terra/raster compatibility
  if (inherits(path, "character")) {
    # Try terra first for better performance
    tryCatch({
      rastermap <- terra::rast(path)
      if (terra::nlyr(rastermap) > 1) {
        rastermap <- rastermap[[1]]  # Use first layer if multilayer
      }
    }, error = function(e) {
      # Fallback to raster
      warning("Terra loading failed, falling back to raster package")
      rastermap <- raster::raster(path)
    })
  } else if (inherits(path, c("RasterLayer", "SpatRaster"))) {
    rastermap <- path
  } else if (inherits(path, c("RasterStack", "RasterBrick"))) {
    rastermap <- path[[1]]  # Use first layer
  } else if (inherits(path, "SpatRaster")) {
    rastermap <- path[[1]]  # Use first layer if multilayer
  } else {
    stop("Input must be a character path, RasterLayer, SpatRaster, or raster stack/brick")
  }
  
  # Get pixel values using appropriate method
  if (inherits(rastermap, "SpatRaster")) {
    # Use terra for SpatRaster - more efficient
    value_map <- terra::freq(rastermap, digits = 0)
    
    # Convert to tibble format
    if (is.data.frame(value_map)) {
      tbfinal <- dplyr::tibble(
        pixvalue = value_map$value,
        Qt = value_map$count
      )
    } else {
      # Handle case where freq returns matrix
      tbfinal <- dplyr::tibble(
        pixvalue = as.numeric(rownames(value_map)),
        Qt = as.numeric(value_map[, 1])
      )
    }
  } else {
    # Use raster for RasterLayer - but vectorized approach
    values_vec <- raster::values(rastermap)
    values_vec <- values_vec[!is.na(values_vec)]  # Remove NA values
    
    # Use table for frequency counting (vectorized)
    value_map <- table(values_vec)
    
    # Convert to tibble efficiently (vectorized)
    tbfinal <- dplyr::tibble(
      pixvalue = as.numeric(names(value_map)),
      Qt = as.numeric(value_map)
    )
  }
  
  return(tbfinal)
}



#' Accumulates changes in a LULC raster time series with terra/raster compatibility
#'
#' This function calculates the number of times a pixel has changed during the 
#' analysed period, providing comprehensive spatial screening of Land Use and Cover 
#' Change (LUCC) frequencies. It returns a raster with the number of changes as pixel 
#' values and a statistical table containing the areal percentage of every change frequency.
#'
#' The function automatically detects and handles both modern terra (SpatRaster) and 
#' legacy raster (RasterStack/RasterBrick) objects, applying appropriate processing 
#' methods for optimal performance while maintaining full backward compatibility.
#'
#' @param path The path for the Raster* directory, RasterStack/RasterBrick object, 
#' SpatRaster object, or list of Raster*/SpatRaster objects to be analysed. 
#' Supports both raster and terra package formats with automatic detection.
#'
#' @details
#' **Processing Logic:**
#' \itemize{
#'   \item Compares consecutive time steps in the series (t1 vs t2, t2 vs t3, etc.)
#'   \item Creates binary change maps (1 = change, 0 = no change) for each interval
#'   \item Sums all change maps to get total change frequency per pixel
#'   \item Generates statistical summary of change patterns across the landscape
#' }
#' 
#' **Terra/Raster Compatibility:**
#' \itemize{
#'   \item **Terra objects**: Uses terra::nlyr(), terra::app(), and optimized layer extraction
#'   \item **Raster objects**: Uses raster::unstack(), raster::overlay(), and legacy operations  
#'   \item **Automatic detection**: Function determines object type and applies appropriate methods
#'   \item **Performance**: Terra processing typically 2-3x faster for large datasets
#' }
#' 
#' **Change Detection Method:**
#' For each consecutive pair of time steps, the function:
#' 1. Extracts individual layers from the stack/raster collection
#' 2. Applies pixel-wise comparison using conditional logic
#' 3. Creates binary change indicators (ifelse(pixel_t1 != pixel_t2, 1, 0))
#' 4. Accumulates all change indicators to generate final change frequency map
#'
#' @return A list containing two objects:
#' \itemize{
#'   \item **Element 1**: A RasterLayer/SpatRaster with pixel values representing the 
#'   number of changes (0 = no changes, 1 = one change, 2 = two changes, etc.)
#'   \item **Element 2**: A tibble with three columns:
#'   \enumerate{
#'     \item \code{PxValue}: Number of changes per pixel (0, 1, 2, ...)
#'     \item \code{Qt}: Quantity of pixels with that change frequency
#'     \item \code{Percent}: Percentage of total area with that change frequency
#'   }
#' }
#' @export
#'
#'
#'
#' @examples
#' \donttest{
#' url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
#' temp <- tempfile()
#' download.file(url, temp, mode = "wb") # downloading the SaoLourencoBasin dataset
#' load(temp)
#' # the acc_changes() function, with the SaoLourencoBasin dataset
#' acc_changes(SaoLourencoBasin)
#' }
#'

acc_changes <- function(path) {

  rList <- .input_rasters(path)

  # Handle both terra and raster objects for unstacking
  if (inherits(rList, "SpatRaster")) {
    # For terra objects, convert to list of individual layers
    n_raster <- terra::nlyr(rList)
    if (n_raster < 2) {
      stop('acc_changes needs at least 2 rasters')
    }
    rList <- lapply(1:n_raster, function(i) rList[[i]])
  } else {
    # For raster objects, use unstack
    rList <- raster::unstack(rList)
    n_raster <- length(rList)
    if (n_raster < 2) {
      stop('acc_changes needs at least 2 rasters')
    }
  }

  # Create difference layers - handle both terra and raster objects
  difflist <- mapply(
    function(x, y) {
      if (inherits(x, "SpatRaster") && inherits(y, "SpatRaster")) {
        # For terra objects, use terra::app with combined rasters
        combined <- c(x, y)  # Use base R c() function, not terra::c()
        terra::app(combined, fun = function(vals) {
          # vals is a matrix with columns for each layer
          ifelse(vals[,1] != vals[,2], 1, 0)
        })
      } else {
        # For raster objects, use raster::overlay
        raster::overlay(
          x,
          y,
          fun = function(x1, x2)
            ifelse((x1 != x2), 1, 0)
        )
      }
    },
    x = rList[1:(length(rList) - 1)],
    y = rList[2:length(rList)],
    SIMPLIFY = FALSE
  )

  # Sum all difference layers
  if (inherits(difflist[[1]], "SpatRaster")) {
    # For terra objects
    sumraster <- Reduce("+", difflist)
  } else {
    # For raster objects
    sumraster <- sum(raster::stack(difflist))
  }

  Freq <- Var1 <- NULL

  # Extract values for table - handle both terra and raster objects
  if (inherits(sumraster, "SpatRaster")) {
    # For terra objects
    df01_values <- table(terra::values(sumraster, na.rm = TRUE))
  } else {
    # For raster objects
    df01_values <- table(matrix(sumraster))
  }

  df_values <- dplyr::mutate(data.frame(df01_values),
                             Var1 = as.character(Var1),
                             Var1 = as.integer(Var1),
                             Percent = Freq/sum(Freq)*100)

  df_values <- dplyr::as_tibble(df_values)

  names(df_values) <- c("PxValue", "Qt", "Percent")

  list(sumraster, df_values)

}


#' Extract year from raster name with flexible naming conventions
#'
#' This internal function provides flexible extraction of years from raster names
#' supporting different separators, year positions, and custom patterns.
#'
#' @param name character. The raster name to extract year from.
#' @param separator character. The separator used to split the name.
#' @param position character or numeric. Position of the year in the split name.
#' @param pattern character. Regular expression pattern to extract year.
#'
#' @return character. The extracted year as a string.
#'
#' @keywords internal
#' @noRd
extract_year_from_name <- function(name, separator = "_", 
                                   position = "last", pattern = NULL) {
  if (!is.null(pattern)) {
    # Use custom pattern if provided
    year_match <- stringr::str_extract(name, pattern)
    if (is.na(year_match)) {
      stop(paste("Could not extract year from name:", name, "using pattern:", pattern))
    }
    return(year_match)
  } else {
    # Auto-detect R modifications and adjust separator
    original_separator <- separator
    
    # Handle common R name modifications
    if (separator == "-" && !grepl("-", name) && grepl("\\.", name)) {
      separator <- "."
      warning(paste("R converted hyphens to dots in raster names. Using '.' instead of '-' for:", name))
    }
    
    # Remove R's automatic "X" prefix if present when extracting from beginning
    clean_name <- name
    x_prefix_removed <- FALSE
    if (position == "first" && grepl("^X[0-9]", name)) {
      clean_name <- sub("^X", "", name)
      x_prefix_removed <- TRUE
      warning(paste("R added 'X' prefix to numeric name. Removing 'X' from:", name))
    }
    
    # Use separator and position
    parts <- strsplit(clean_name, separator, fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      # Enhanced error message with suggestions
      suggested_separators <- c("_", ".", "-")
      found_separators <- suggested_separators[sapply(suggested_separators, function(s) grepl(s, name, fixed = TRUE))]
      
      error_msg <- paste("Name", name, "does not contain separator", original_separator)
      if (length(found_separators) > 0) {
        error_msg <- paste(error_msg, "\nFound these separators in the name:", paste(found_separators, collapse = ", "))
        error_msg <- paste(error_msg, "\nTry using name_separator =", paste0("'", found_separators[1], "'"))
      }
      if (grepl("[0-9]{4}", name)) {
        error_msg <- paste(error_msg, "\nAlternatively, use name_pattern = '[0-9]{4}' to extract the year directly")
      }
      stop(error_msg)
    }
    
    if (position == "last") {
      year <- parts[length(parts)]
    } else if (position == "first") {
      year <- parts[1]
    } else if (is.numeric(position)) {
      if (position > length(parts) || position < 1) {
        stop(paste("Position", position, "is out of range for name:", name, "with", length(parts), "parts"))
      }
      year <- parts[position]
    } else {
      stop("year_position must be 'first', 'last', or a numeric position")
    }
    
    # Validate that extracted part looks like a year
    if (!grepl("^[0-9]{4}$", year)) {
      # Enhanced error with suggestions
      error_msg <- paste("Extracted part '", year, "' from name '", name, "' does not look like a 4-digit year", sep = "")
      if (grepl("[0-9]{4}", name)) {
        found_years <- stringr::str_extract_all(name, "[0-9]{4}")[[1]]
        error_msg <- paste(error_msg, "\nFound these 4-digit numbers:", paste(found_years, collapse = ", "))
        error_msg <- paste(error_msg, "\nConsider using name_pattern = '[0-9]{4}' instead")
      }
      stop(error_msg)
    }
    
    return(year)
  }
}


#' Check terra availability and performance status
#'
#' Helper function to check if terra package is available and provide
#' performance recommendations for OpenLand functions.
#'
#' @return A list with terra availability status and performance tips
#' @export
#'
#' @examples
#' \dontrun{
#' performance_status()
#' }
#'
performance_status <- function() {
  terra_available <- requireNamespace("terra", quietly = TRUE)
  
  result <- list(
    terra_available = terra_available,
    recommendations = character(0),
    performance_tips = character(0)
  )
  
  if (terra_available) {
    result$recommendations <- c(
      "✓ Terra package is available - functions will use terra for better performance",
      "✓ Expected performance improvement: 2-10x faster for large raster datasets",
      "✓ Memory usage optimization: terra handles large rasters more efficiently"
    )
    
    result$performance_tips <- c(
      "• Use summary_dir() with terra objects for fastest directory summaries",
      "• summary_map() automatically detects and uses terra::freq() for faster pixel counting",
      "• .input_rasters() defaults to terra format - set use_terra=FALSE to force raster format",
      "• For very large datasets, terra's out-of-memory processing provides significant advantages"
    )
  } else {
    result$recommendations <- c(
      "⚠ Terra package not available - falling back to raster package",
      "⚠ Consider installing terra for significant performance improvements:",
      "  install.packages('terra')",
      "⚠ Performance may be slower for large raster datasets"
    )
    
    result$performance_tips <- c(
      "• Install terra package: install.packages('terra')",
      "• Terra provides 2-10x performance improvements for raster operations",
      "• Terra has better memory management for large datasets",
      "• Terra supports larger-than-memory raster processing"
    )
  }
  
  # Print formatted output
  cat("OpenLand Performance Status\n")
  cat("==========================\n\n")
  
  cat("Status:\n")
  for (rec in result$recommendations) {
    cat(rec, "\n")
  }
  
  cat("\nPerformance Tips:\n")
  for (tip in result$performance_tips) {
    cat(tip, "\n")
  }
  
  cat("\nFor more information, see: ?summary_dir, ?summary_map, ?.input_rasters\n")
  
  invisible(result)
}


#' Performance benchmarking for OpenLand functions
#'
#' Benchmarks key OpenLand functions to measure performance improvements and
#' help users optimize their workflows. Compares terra vs raster performance
#' and provides timing measurements for different optimization settings.
#'
#' @param test_data Optional test dataset. If NULL, generates synthetic test data.
#' @param n_layers Integer. Number of raster layers to generate for testing (default: 3).
#' @param matrix_size Integer. Size of test raster matrix (default: 100 for 100x100 pixels).
#' @param run_full_benchmark Logical. If TRUE, runs comprehensive benchmarks 
#' including contingencyTable analysis (default: FALSE for quick tests).
#'
#' @return A list containing benchmark results and performance recommendations
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick performance check
#' benchmark_performance()
#' 
#' # Full benchmark with larger test data
#' benchmark_performance(matrix_size = 200, run_full_benchmark = TRUE)
#' }
#'
benchmark_performance <- function(test_data = NULL, n_layers = 3, matrix_size = 100, 
                                 run_full_benchmark = FALSE) {
  
  cat("OpenLand Performance Benchmark\n")
  cat("=============================\n\n")
  
  # Check package availability
  terra_available <- requireNamespace("terra", quietly = TRUE)
  future_available <- requireNamespace("future.apply", quietly = TRUE)
  
  results <- list(
    terra_available = terra_available,
    future_available = future_available,
    benchmarks = list(),
    recommendations = character(0)
  )
  
  # Generate test data if not provided
  if (is.null(test_data)) {
    cat("Generating synthetic test data...\n")
    
    # Create test matrices with realistic land use classes (1-5)
    test_matrices <- lapply(1:n_layers, function(i) {
      set.seed(123 + i)  # Reproducible random data
      matrix(sample(1:5, matrix_size^2, replace = TRUE), 
             nrow = matrix_size, ncol = matrix_size)
    })
    
    if (terra_available) {
      # Use terra for test data creation
      test_rasters_terra <- lapply(seq_along(test_matrices), function(i) {
        r <- terra::rast(test_matrices[[i]])
        names(r) <- paste0("test_", 2020 + i - 1)
        r
      })
      test_stack_terra <- do.call(c, test_rasters_terra)
      
      cat("✓ Generated", n_layers, "test rasters using terra (",
          matrix_size, "x", matrix_size, "pixels each)\n")
    }
    
    # Always create raster package versions for comparison
    if (requireNamespace("raster", quietly = TRUE)) {
      test_rasters_raster <- lapply(seq_along(test_matrices), function(i) {
        r <- raster::raster(test_matrices[[i]])
        names(r) <- paste0("test_", 2020 + i - 1)
        r
      })
      test_stack_raster <- raster::stack(test_rasters_raster)
      
      cat("✓ Generated", n_layers, "test rasters using raster package\n")
    }
  }
  
  cat("\n--- Benchmarking Core Functions ---\n")
  
  # 1. Test summary_dir performance
  if (exists("test_rasters_terra") && exists("test_rasters_raster")) {
    cat("\n1. summary_dir() performance:\n")
    
    # Terra version
    if (terra_available) {
      time_terra <- system.time({
        summary_terra <- summary_dir(test_rasters_terra)
      })
      cat("   Terra version: ", round(time_terra[3], 4), "seconds\n")
      results$benchmarks$summary_dir_terra <- time_terra[3]
    }
    
    # Raster version  
    time_raster <- system.time({
      summary_raster <- summary_dir(test_rasters_raster)
    })
    cat("   Raster version:", round(time_raster[3], 4), "seconds\n")
    results$benchmarks$summary_dir_raster <- time_raster[3]
    
    if (terra_available) {
      speedup <- time_raster[3] / time_terra[3]
      cat("   Speedup with terra:", round(speedup, 2), "x faster\n")
      results$benchmarks$summary_dir_speedup <- speedup
    }
  }
  
  # 2. Test summary_map performance
  cat("\n2. summary_map() performance:\n")
  
  if (exists("test_rasters_terra")) {
    time_terra_map <- system.time({
      map_terra <- summary_map(test_rasters_terra[[1]])
    })
    cat("   Terra version: ", round(time_terra_map[3], 4), "seconds\n")
    results$benchmarks$summary_map_terra <- time_terra_map[3]
  }
  
  if (exists("test_rasters_raster")) {
    time_raster_map <- system.time({
      map_raster <- summary_map(test_rasters_raster[[1]])
    })
    cat("   Raster version:", round(time_raster_map[3], 4), "seconds\n")
    results$benchmarks$summary_map_raster <- time_raster_map[3]
  }
  
  if (exists("time_terra_map") && exists("time_raster_map")) {
    speedup_map <- time_raster_map[3] / time_terra_map[3]
    cat("   Speedup with terra:", round(speedup_map, 2), "x faster\n")
    results$benchmarks$summary_map_speedup <- speedup_map
  }
  
  # 3. Test contingencyTable performance (if requested)
  if (run_full_benchmark) {
    cat("\n3. contingencyTable() performance (full analysis):\n")
    
    if (exists("test_stack_terra")) {
      time_contingency_terra <- system.time({
        cont_terra <- contingencyTable(test_stack_terra, pixelresolution = 30, 
                                     parallel = future_available)
      })
      cat("   Terra version: ", round(time_contingency_terra[3], 4), "seconds\n")
      results$benchmarks$contingencyTable_terra <- time_contingency_terra[3]
    }
    
    if (exists("test_stack_raster")) {
      time_contingency_raster <- system.time({
        cont_raster <- contingencyTable(test_stack_raster, pixelresolution = 30, 
                                       parallel = FALSE)  # Disable parallel for fair comparison
      })
      cat("   Raster version:", round(time_contingency_raster[3], 4), "seconds\n")
      results$benchmarks$contingencyTable_raster <- time_contingency_raster[3]
    }
    
    if (exists("time_contingency_terra") && exists("time_contingency_raster")) {
      speedup_cont <- time_contingency_raster[3] / time_contingency_terra[3]
      cat("   Speedup with terra:", round(speedup_cont, 2), "x faster\n")
      results$benchmarks$contingencyTable_speedup <- speedup_cont
    }
  }
  
  # Generate performance recommendations
  cat("\n--- Performance Recommendations ---\n")
  
  if (terra_available) {
    results$recommendations <- c(
      "✓ Terra package detected - significant performance improvements available",
      "• Terra provides 2-10x speedup for most raster operations"
    )
    
    if (results$benchmarks$summary_dir_speedup > 2) {
      results$recommendations <- c(results$recommendations,
        paste("• summary_dir() is", round(results$benchmarks$summary_dir_speedup, 1), 
              "x faster with terra"))
    }
    
    if (results$benchmarks$summary_map_speedup > 2) {
      results$recommendations <- c(results$recommendations,
        paste("• summary_map() is", round(results$benchmarks$summary_map_speedup, 1), 
              "x faster with terra"))
    }
  } else {
    results$recommendations <- c(
      "⚠ Install terra package for significant performance improvements:",
      "  install.packages('terra')",
      "• Expected 2-10x speedup for raster operations"
    )
  }
  
  if (future_available) {
    results$recommendations <- c(results$recommendations,
      "✓ future.apply available - parallel processing enabled for large datasets")
  } else {
    results$recommendations <- c(results$recommendations,
      "⚠ Install future.apply for parallel processing:",
      "  install.packages('future.apply')")
  }
  
  # Print recommendations
  for (rec in results$recommendations) {
    cat(rec, "\n")
  }
  
  cat("\nBenchmark completed. Use benchmark_performance(run_full_benchmark = TRUE)")
  cat("\nfor comprehensive analysis with contingencyTable().\n")
  
  invisible(results)
}
