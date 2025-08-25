utils::globalVariables(c("Interval", "Period", "Year_from",
                         "Year_to", "strings01", "strings02"))

#' @include demolandscape.R rasters_input.R generalfunctions.R
NULL

#' Contingency table
#'
#'
#' Extracts LUC transitions for all input grids of the time series.
#'
#' @param input_raster path (character), Raster* object or list of Raster*
#' objects. See \cr \code{\link[raster]{raster}} for more information about
#' supported file types.
#' @param pixelresolution numeric. The pixel spatial resolution in meter.
#' @param name_separator character. The separator used to split the raster names. 
#' Default is "_" (underscore).
#' @param year_position character. Position of the year in the split name. 
#' Options: "last" (default), "first", or a numeric position.
#' @param name_pattern character. Regular expression pattern to extract year from names.
#' If provided, overrides name_separator and year_position. Default is NULL.
#'
#' @details
#' The function provides flexible naming conventions for input rasters:
#' \itemize{
#'   \item Default: "text_YEAR" format (e.g., "landscape_2020")
#'   \item Custom separator: Use \code{name_separator} for different separators
#'   \item Year position: Use \code{year_position} to specify where the year appears
#'   \item Pattern matching: Use \code{name_pattern} for complex naming schemes
#' }
#'
#' @import dplyr
#' @importFrom stringr str_extract
#'
#' @return A list that contains 5 objects.
#' \itemize{
#'   \item \code{lulc_Mulstistep}: \code{<tibble>} Contingency table for all
#'   analysed time steps, containing 8 columns:
#'   \enumerate{
#'   \item Period: \code{<chr>} The period \emph{[Yt, Yt+1]}.
#'   \item From: \code{<dbl>} numerical code of a LUC category \emph{i}.
#'   \item To: \code{<dbl>} numerical code of a LUC category \emph{j}.
#'   \item km2: \code{<dbl>} Area in square kilometers that transited from the
#'   category \emph{i}
#'    to category \emph{j} in the period from \emph{Yt} to \emph{Yt+1}.
#'   \item Interval: \code{<dbl>} Interval of years between the first and
#'    the last year of the period \emph{[Yt, Yt+1]}.
#'   \item QtPixel: \code{<int>} Pixel count that transited from the categories
#'    \emph{i}
#'    to category \emph{j} in the period from \emph{Yt} to \emph{Yt+1}.
#'   \item yearFrom: \code{<chr>} The year that the change comes from \emph{[Yt]}.
#'   \item yearTo: \code{<chr>} The year that the change goes for \emph{[Yt+1]}.
#'   }
#'   \item \code{lulc_Onestep}:\code{<tibble>} Contingency table for the entire
#'   analysed period \emph{[Y1, YT]}, containing
#'   8 columns identical with \code{lulc_Mulstistep}.
#'   \item \code{tb_legend}: \code{<tibble>} A table of the pixel value, his
#'   name and color containing 3 columns:
#'   \enumerate{
#'   \item categoryValue: \code{<dbl>} the pixel value of the LUC category.
#'   \item categoryName: \code{<factor>} randomly created string associated with
#'    a given pixel value of a LUC category.
#'   \item color: \code{<chr>} random color associated with the given pixel value
#'    of a LUC category.
#'   Before further analysis, one would like to change the \code{categoryName}
#'   and \code{color} values.
#'     \itemize{
#'         \item Therefore the category names have to be in the same order as the
#'          \code{categoryValue}
#'         and the \code{levels} should be put in the right order for legend
#'         plotting. Like:
#'         \preformatted{
#'
#'         myobject$tb_legend$categoryName <- factor(c("name1", "name2", "name3", "name4"),
#'                                                levels = c("name3", "name2", "name1", "name4"))}
#'         \item The colors have to in the same order as the values in the \code{categoryValue} column. Colors can be given by the
#'        color name (eg. "black") or an HEX value (eg. #FFFFFF). Like:
#'        \preformatted{
#'
#'        myobject$tb_legend$color <- c("#CDB79E", "red", "#66CD00", "yellow")}}}
#'   \item \code{totalArea}: \code{<tibble>}  A table with the total area of the
#'    study area containing 2 columns:
#'   \enumerate{
#'   \item area_km2: \code{<numeric>} The total area in square kilometers.
#'   \item QtPixel: \code{<numeric>} The total area in pixel counts.
#'   }
#'   \item \code{totalInterval}: \code{<numeric>} Total interval of the analysed
#'    time series in years.
#'   }
#'
#'
#' @export
#'
#' @importFrom raster unstack crosstab compareRaster raster values stack overlay brick
#'
#' @examples
#' \donttest{
#' url <- "https://zenodo.org/record/3685230/files/SaoLourencoBasin.rda?download=1"
#' temp <- tempfile()
#' download.file(url, temp, mode = "wb") #downloading the online dataset
#' load(temp)
#' 
#' # Basic usage with default naming convention (text_YEAR)
#' contingencyTable(input_raster = SaoLourencoBasin, pixelresolution = 30)
#' 
#' # Using different separator (e.g., for names like "landscape-2020")
#' # contingencyTable(input_raster = your_raster, name_separator = "-")
#' 
#' # Using year at the beginning (e.g., "2020_landscape_data")
#' # contingencyTable(input_raster = your_raster, year_position = "first")
#' 
#' # Using custom pattern to extract year from complex names
#' # contingencyTable(input_raster = your_raster, name_pattern = "\\d{4}")
#' }
#'
#'

contingencyTable <-
  function(input_raster, pixelresolution = 30, name_separator = "_", 
           year_position = "last", name_pattern = NULL) {

    rList <- .input_rasters(input_raster)

    n_raster <- raster::nlayers(rList)

    if (n_raster < 2) {
      stop('contingencyTable needs at least 2 rasters')
    }

    # compute the cross table of two rasters, then setting the columns name
    table_cross <- function(x, y) {
      contengency <- raster::crosstab(x, y, long = TRUE, progress = "text")
      
      # Extract years from raster names using the flexible function
      name_x <- names(x)
      name_y <- names(y)
      year_from <- extract_year_from_name(name_x, name_separator, year_position, name_pattern)
      year_to <- extract_year_from_name(name_y, name_separator, year_position, name_pattern)
      
      # Create standardized names for the cross-tabulation
      from_name <- paste0("from_", year_from)
      to_name <- paste0("to_", year_to)
      
      contengency %>% dplyr::mutate(Year_from = from_name,
                                    Year_to = to_name) %>%
        dplyr::rename(
          From = colnames(contengency)[1],
          To = colnames(contengency)[2],
          QtPixel = colnames(contengency)[3]
        ) %>% dplyr::mutate(From = as.integer(From), To = as.integer(To))
    }


    table_one <- table_cross(rList[[1]], rList[[raster::nlayers(rList)]])

    if (raster::nlayers(rList) == 2) {
      table_multi <- table_one
    }
    else {
      rList_multi <- raster::unstack(rList)
      table_multi <- Reduce(rbind,
                            mapply(function(x, y)
                              table_cross(x, y), rList_multi[1:(length(rList_multi) - 1)],
                              rList_multi[2:length(rList_multi)], SIMPLIFY = FALSE))
    }


    lulc <- list(oneStep = table_one, multiStep = table_multi)

    lulctable <-
      lapply(lulc, function(x)
        x %>% dplyr::arrange(Year_from) %>%
          dplyr::mutate(
            yearFrom = as.integer(stringr::str_extract(Year_from, "[0-9]{4}")),
            yearTo = as.integer(stringr::str_extract(Year_to, "[0-9]{4}"))
          ) %>%
          dplyr::mutate(Interval = yearTo - yearFrom) %>%
          dplyr::mutate(km2 = QtPixel * (pixelresolution ^ 2) / 1e+06) %>%
          tidyr::unite("Period", c("yearFrom", "yearTo"), sep = "-", remove = FALSE) %>%
          dplyr::select(Period, From, To, km2, QtPixel, Interval, yearFrom, yearTo))


    allinterval <- # calculating the total interval and the pixelValue
      as.numeric(dplyr::last(lulctable[[2]]$yearTo)) - as.numeric(dplyr::first(lulctable[[2]]$yearFrom))

    tb_legend <-
      lulctable[[2]] %>% dplyr::distinct(From) %>% dplyr::rename(categoryValue = From)

    genCategory <- function() {paste(sample(LETTERS, size = 3, replace = FALSE), collapse = "")}

    tb_legend$categoryName <- as.factor(vapply(seq_len(nrow(tb_legend)), function(x) genCategory(), character(1)))

    tb_legend$color <- base::sample(x = c("#002F70", "#0A468D", "#295EAE", "#4A76C7", "#6F8DD2",
                                          "#8EA4DE", "#ABBBE8", "#C5CFF0", "#DCE2F6", "#EFF1F8",
                                          "#F9EFEF", "#F9DCDC", "#F3C5C5", "#EAACAC", "#DD9191",
                                          "#CE7575", "#BD5758", "#A13F3F", "#7F2A2B", "#5F1415"),
                                    size = nrow(tb_legend),
                                    replace = (nrow(tb_legend) > 20))


    areaTotal <-
      lulctable[[2]] %>% dplyr::group_by(Period) %>% dplyr::summarise(area_km2 = sum(km2), QtPixel = sum(QtPixel))

    contingencyTable <-
      list(
        lulc_Multistep = dplyr::as_tibble(lulctable[[2]]),
        lulc_Onestep = dplyr::as_tibble(lulctable[[1]]),
        tb_legend = dplyr::as_tibble(tb_legend),
        totalArea = areaTotal[1, c(2,3)],
        totalInterval = allinterval
      )
    return(contingencyTable)
  }
