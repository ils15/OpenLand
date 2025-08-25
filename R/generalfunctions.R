
#' Summary of multiple parameters in a raster directory
#'
#' Listing major characteristics of raster inputs. Those characteristics are the
#' dimensions, the resolution, the extent, the values (min, max) and the
#' coordinate reference system.
#'
#' @param path The path for the Raster* directory or list of Raster* to be
#' analysed.
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
#' # the acc_changes() function, with the SaoLourencoBasin dataset
#'
#' summary_dir(raster::unstack(SaoLourencoBasin))
#' }
#'
summary_dir <- function(path) {

  if (inherits(path, "list") &
      inherits(path[[1]], "RasterLayer")) {
    layer_list <- path
  } else if (inherits(path, "character")) {

    raster_files <-
      list.files(path,
                 pattern = ".tif$",
                 full.names = TRUE)

    layer_list <- vector("list", length = length(raster_files))


    for (i in seq_along(raster_files)) {
      layer_list[[i]] <- raster::raster(raster_files[i])
    }
  }

  Reduce(rbind,
         lapply(layer_list, function(x) {
           layermap <- x
           tibble(
             file_name = base::names(x),
             xmin = raster::xmin(layermap),
             xmax = raster::xmax(layermap),
             ymin = raster::ymin(layermap),
             ymax = raster::ymax(layermap),
             res_x = raster::res(layermap)[1],
             res_y = raster::res(layermap)[2],
             nrow = raster::nrow(layermap),
             ncol = raster::ncol(layermap),
             min_val = raster::minValue(layermap),
             max_val = raster::maxValue(layermap),
             crs = as.character(raster::crs(layermap))
           )
         }))
}


#' Quantitative summary of a unique categorical raster
#'
#' This function presents a summary with the pixel quantity of each category
#' present in a categorical raster.
#'
#' @param path The path for the raster to be analysed, if path is a multilayer
#' raster only the first RasterLayer will be analysed.
#'
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
  rastermap <-
    if (!inherits(path, "character")) {
      if (inherits(path, "RasterLayer")) {
        path
      } else {
        path[[1]]
      }
    } else {
      raster::raster(path)
    }
  value_map <- table(raster::values(rastermap))

  tbfinal <- dplyr::tibble(pixvalue = numeric(length(value_map)),
                           Qt = numeric(length(value_map)))

  for (i in seq_along(value_map)) {
    tbfinal[i, c(1:2)] <-
      list(as.numeric(names(value_map)[i]), value_map[[i]])
  }
  return(tbfinal)
}



#' Accumulates changes in a LULC raster time series
#'
#' This function calculates the number of times a pixel has changed during
#' the analysed period. It returns a raster with the number of changes as
#' pixel value and a table containing the areal percentage of every pixel value
#' (number of changes).
#'
#'
#' @param path The path for the Raster* directory or list of Raster* to be
#' analysed.
#'
#'
#' @return Two objects, a RasterLayer and a table.
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

  rList <- raster::unstack(rList)

  n_raster <- length(rList)

  if (n_raster < 2) {
    stop('acc_changes needs at least 2 rasters')
  }

  difflist <- mapply(
    function(x, y)
      raster::overlay(
        x,
        y,
        fun = function(x1, x2)
          ifelse((x1 != x2), 1, 0)
      ),
    x = rList[1:(length(rList) - 1)],
    y = rList[2:length(rList)],
    SIMPLIFY = FALSE
  )

  sumraster <- sum(raster::stack(difflist))

  Freq <- Var1 <- NULL

  df01_values <- table(matrix(sumraster))

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
    # Use separator and position
    parts <- strsplit(name, separator, fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      stop(paste("Name", name, "does not contain separator", separator))
    }
    
    if (position == "last") {
      year <- parts[length(parts)]
    } else if (position == "first") {
      year <- parts[1]
    } else if (is.numeric(position)) {
      if (position > length(parts) || position < 1) {
        stop(paste("Position", position, "is out of range for name:", name))
      }
      year <- parts[position]
    } else {
      stop("year_position must be 'first', 'last', or a numeric position")
    }
    
    # Validate that extracted part looks like a year
    if (!grepl("^[0-9]{4}$", year)) {
      stop(paste("Extracted part", year, "from name", name, "does not look like a 4-digit year"))
    }
    
    return(year)
  }
}
