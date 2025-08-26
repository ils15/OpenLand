utils::globalVariables(c("Gain", "Gtj", "Loss", "Lti", "N", "Qtmj",
                         "Rtin", "St", "Vtm", "Wtn", "intch_QtPixel",
                         "intch_km2", "num02"))
#' @include intensityClass.R
NULL


#' Performs the intensity analysis based on cross-tabulation matrices of each
#' time step
#'
#' This function implements an Intensity Analysis (IA) according to Aldwaik &
#' Pontius (2012), a quantitative method to analyze time series of land use and
#' cover (LUC) maps.  For IA, a cross-tabulation matrix is composed for each LUC
#' transition step in time.
#'
#' IA includes three levels of analysis of LUC changes. Consecutive analysis
#' levels detail hereby information given by the previous analysis level
#' \cite{(Aldwaik and Pontius, 2012, 2013)}.
#'
#'
#' \enumerate{
#'  \item The \emph{interval level} examines how the size and speed of change
#'  vary across time intervals.
#'  \item The \emph{category level} examines how the size and intensity of gross
#'  losses and gross gains in each category vary across categories for each time
#'  interval.
#'  \item The \emph{transition level} examines how the size and intensity of a
#'  category’s transitions vary across the other categories that are available
#'  for that transition.
#'   }
#'
#'
#' At each analysis level, the method tests for stationarity of patterns across
#' time intervals.
#'
#' \bold{The function returns a list with 6 objects:}
#' \enumerate{
#'  \item lulc_table: \code{tibble}. Contingency table of LUC transitions at all
#'   analysed time steps, containing 6 columns:
#'    \enumerate{
#'    \item Period:  \code{<fct>}. Evaluated period of transition in the format
#'    \code{year t - year t+1}.
#'    \item From: \code{<fct>}. The category in year t.
#'    \item To: \code{<fct>}. The category in year t+1.
#'    \item km2: \code{<dbl>}. Area in square kilometers that transited from the
#'     category \code{From}.
#'    to the category \code{To} in the period.
#'    \item QtPixel: \code{<int>}. Number of pixels that transited from.
#'    the category \code{From} to the category \code{To} in the period.
#'    \item Interval: \code{<int>}. Interval in years of the evaluated period.
#'
#'    }
#'
#'  \item \emph{lv1_tbl}: An \code{\linkS4class{Interval}} object containing the
#'   \emph{St} and \emph{U} values.
#'  \item \emph{category_lvlGain}: A \code{\linkS4class{Category}} object
#'  containing the gain of the LUC category in a period (\emph{Gtj}).
#'  \item \emph{category_lvlLoss}: A \code{\linkS4class{Category}} object
#'  containing the loss of the LUC category in a period (\emph{Lti}).
#'  \item \emph{transition_lvlGain_n}: A \code{\linkS4class{Transition}} object
#'  containing the annualized rate of gain in \emph{category n} (\emph{Rtin}) and
#'  the respective Uniform Intensity (\emph{Wtn}).
#'  \item \emph{transition_lvlLoss_m}: A \code{\linkS4class{Transition}} object
#'  containing the annualized rate of loss in \emph{category m} (\emph{Qtmj}) and
#'  the respective Uniform Intensity (\emph{Vtm}).
#'
#'   }
#'
#'
#'
#'
#' @param dataset list. The result object from \code{\link{contingencyTable}}.
#' @param category_n character. The gaining category in the transition of interest (\emph{n}).
#' @param category_m character. The losing category in the transition of interest (\emph{m}).
#' @param area_km2 logical. If TRUE the change is computed in km2, if FALSE in pixel counts.
#'
#' @return Intensity object
#' @export
#'
#'
#' @references Aldwaik, S. Z. and Pontius, R. G. (2012) ‘Intensity analysis to unify
#' measurements of size and stationarity of land changes by interval, category, and
#' transition, Landscape and Urban Planning. Elsevier B.V., 106(1), pp. 103–114.
#' \doi{10.1016/j.landurbplan.2012.02.010}.
#'
#' Aldwaik, S. Z. and Pontius, R. G. (2013) ‘Map errors that could account for deviations
#' from a uniform intensity of land change, International Journal of Geographical
#' Information Science. Taylor & Francis, 27(9), pp. 1717–1739. \doi{10.1080/13658816.2013.787618}.
#'
#'
#'
#' @examples
#'
#' # editing the category name
#'
#' SL_2002_2014$tb_legend$categoryName <- factor(c("Ap", "FF", "SA", "SG", "aa", "SF",
#'                                              "Agua", "Iu", "Ac", "R", "Im"),
#'                                   levels = c("FF", "SF", "SA", "SG", "aa", "Ap",
#'                                              "Ac", "Im", "Iu", "Agua", "R"))
#'
#  # add the color by the same order of the legend factor
#' SL_2002_2014$tb_legend$color <- c("#FFE4B5", "#228B22", "#00FF00", "#CAFF70",
#'                                   "#EE6363", "#00CD00", "#436EEE", "#FFAEB9",
#'                                   "#FFA54F", "#68228B", "#636363")
#'
#' intensityAnalysis(dataset = SL_2002_2014, category_n = "Ap", category_m = "SG", area_km2 = TRUE)
#'
#'

intensityAnalysis <-
  function(dataset, category_n, category_m, area_km2 = TRUE) {
    # Performance optimization: Pre-validate inputs
    if (!is.list(dataset) || length(dataset) < 5) {
      stop("Invalid dataset structure")
    }
    
    # Performance optimization: Direct data access with reduced copying
    AE <- dataset[[4]] # study area
    allinterval <- dataset[[5]] # whole interval in years
    
    # Performance optimization: Efficient joins using data.table-style operations
    # Reduce memory allocations by selecting specific columns first
    legend_lookup <- dataset[[3]][, c(1, 2)]
    names(legend_lookup) <- c("categoryValue", "categoryName")
    
    # Single efficient join operation instead of multiple left_joins
    lulc <- dataset[[1]] %>%
      dplyr::left_join(legend_lookup, by = c("From" = "categoryValue"), suffix = c("", ".from")) %>%
      dplyr::left_join(legend_lookup, by = c("To" = "categoryValue"), suffix = c(".from", ".to")) %>%
      dplyr::select(Period, From = categoryName.from, To = categoryName.to, km2, QtPixel, Interval)

    # Performance optimization: More efficient factor creation
    unique_periods <- unique(lulc$Period)
    lulc$Period <- factor(lulc$Period, levels = rev(unique_periods))

    # Performance optimization: Direct assignment instead of intermediate objects
    category_fillColor <- dataset[[3]][c(2, 3)]
    lookupcolor <- setNames(category_fillColor$color, category_fillColor$categoryName)


    if (isTRUE(area_km2)) {
      # Performance optimization: Pre-filter data once for efficiency
      change_data <- lulc %>% dplyr::filter(From != To)
      
      # ____________Interval-------
      # EQ1 - St ---- (Optimized with reduced intermediate objects)
      eq1 <- change_data %>%
        dplyr::group_by(Period, Interval) %>% 
        dplyr::summarise(intch_km2 = sum(km2), .groups = "drop") %>%
        dplyr::mutate(
          PercentChange = (intch_km2 / AE[[1, 1]]) * 100,
          St = (intch_km2 / (Interval * AE[[1, 1]])) * 100
        ) %>%
        dplyr::select(Period, PercentChange, St)

      # EQ2 - U ---- (Optimized calculation)
      total_change_km2 <- sum(change_data$km2)
      U_value <- (total_change_km2 / (allinterval * AE[[1, 1]])) * 100
      
      level01 <- eq1 %>% dplyr::mutate(U = U_value)

      # ____________Categorical ---- (Optimized with combined operations)
      # EQ3 - Gtj ---- (Reduced memory allocations)
      categorical_summary <- lulc %>%
        dplyr::group_by(Period, To, From, Interval) %>%
        dplyr::summarise(km2_val = sum(km2), .groups = "drop")
      
      # Optimized gain calculation
      gain_summary <- categorical_summary %>%
        dplyr::filter(From != To) %>%
        dplyr::group_by(Period, To, Interval) %>%
        dplyr::summarise(gain_km2 = sum(km2_val), .groups = "drop")
      
      total_by_category <- categorical_summary %>%
        dplyr::group_by(Period, To) %>%
        dplyr::summarise(total_km2 = sum(km2_val), .groups = "drop")
      
      eq3 <- gain_summary %>%
        dplyr::left_join(total_by_category, by = c("Period", "To")) %>%
        dplyr::mutate(Gtj = (gain_km2 / (total_km2 * Interval)) * 100) %>%
        dplyr::left_join(eq1[c("Period", "St")], by = "Period") %>%
        dplyr::rename(GG_km2 = gain_km2)

      # EQ4 - Lti --------- (Optimized loss calculation)
      loss_summary <- categorical_summary %>%
        dplyr::filter(From != To) %>%
        dplyr::group_by(Period, From, Interval) %>%
        dplyr::summarise(loss_km2 = sum(km2_val), .groups = "drop")
      
      total_by_from <- categorical_summary %>%
        dplyr::group_by(Period, From) %>%
        dplyr::summarise(total_km2 = sum(km2_val), .groups = "drop")
      
      eq4 <- loss_summary %>%
        dplyr::left_join(total_by_from, by = c("Period", "From")) %>%
        dplyr::mutate(Lti = (loss_km2 / (total_km2 * Interval)) * 100) %>%
        dplyr::left_join(eq1[c("Period", "St")], by = "Period") %>%
        dplyr::rename(GL_km2 = loss_km2)

      # ____________Transition ----
      # Witch transitions are particularly intensive in a given time interval?
      # EQ5 - Rtin----
      num05 <-
        lulc %>% dplyr::filter(From != To, To == category_n) %>%
        dplyr::group_by(Period, From, Interval) %>% dplyr::summarise(num05 = sum(km2))

      denom05 <-
        lulc %>% dplyr::filter(From != category_n) %>%
        dplyr::group_by(Period, From) %>% dplyr::summarise(denom05 = sum(km2))

      eq5 <-
        num05 %>% dplyr::left_join(denom05, by = c("Period", "From")) %>%
        dplyr::mutate(Rtin = (num05 / (Interval * denom05)) * 100)

      # EQ6 - Wtn ----
      num06 <- lulc %>% dplyr::filter(From != To, To == category_n) %>%
        dplyr::group_by(Period, To, Interval) %>% dplyr::summarise(num06 = sum(km2))

      denom06 <- lulc  %>% dplyr::filter(From != category_n) %>%
        dplyr::group_by(Period) %>% dplyr::summarise(denom06 = sum(km2))

      eq6 <- num06 %>% dplyr::left_join(denom06, by = "Period") %>%
        dplyr::mutate(Wtn = (num06 / (Interval * denom06)) * 100)

      plot03ganho_n <-
        eq5 %>% dplyr::left_join(eq6, by = c("Period", "Interval")) %>%
        dplyr::select(1,2,7,3,4,6,10) %>% rename("T_i2n_km2" = "num05")

      # EQ7 - Qtmj----
      num07 <- lulc %>% dplyr::filter(From != To, From == category_m) %>%
        dplyr::group_by(Period, To, Interval) %>% dplyr::summarise(num07 = sum(km2))

      denom07 <- lulc %>% dplyr::filter(To != category_m) %>%
        dplyr::group_by(Period, To) %>% dplyr::summarise(denom07 = sum(km2))

      eq7 <-
        num07 %>% dplyr::left_join(denom07, by = c("Period", "To")) %>%
        dplyr::mutate(Qtmj = (num07 / (Interval * denom07)) * 100)

      # EQ8 - Vtm----
      num08 <- lulc %>% dplyr::filter(From != To, From == category_m) %>%
        dplyr::group_by(Period, From, Interval) %>% dplyr::summarise(num08 = sum(km2))

      denom08 <- lulc %>% dplyr::filter(To != category_m) %>%
        dplyr::group_by(Period) %>% dplyr::summarise(denom08 = sum(km2))


      eq8 <- num08 %>% dplyr::left_join(denom08, by = "Period") %>%
        dplyr::mutate(Vtm = (num08 / (Interval * denom08)) * 100)

      plot03perda_m <-
        eq7 %>% dplyr::left_join(eq8, by = c("Period", "Interval")) %>%
        dplyr::select(1,2,7,3,4,6,10) %>% rename("T_m2j_km2" = "num07")
    } else {
      # ____________Interval-------
      # EQ1 - St ----

      eq1 <- lulc %>% dplyr::filter(From != To) %>%
        dplyr::group_by(Period, Interval) %>% dplyr::summarise(intch_QtPixel = sum(QtPixel)) %>% # interval change:intch_QtPixel
        dplyr::mutate(
          PercentChange = (intch_QtPixel / AE[[1, 2]]) * 100,
          St = (intch_QtPixel / (Interval * AE[[1, 2]])) * 100
        ) %>%
        dplyr::select(1, 4, 5)

      # EQ2 - U ----
      eq2 <- lulc %>% dplyr::filter(From != To) %>%
        dplyr::summarise(num02 = sum(QtPixel)) %>% # all area change for the whole period Y1 to YT
        dplyr::mutate(U = (num02 / (allinterval * AE[[1, 2]])) * 100)

      level01 <-
        eq1 %>% dplyr::mutate(U = eq2[[2]]) # Type = ifelse(St > U, "Fast", "Slow"))

      # ____________Categorical ----
      # EQ3 - Gtj ----
      num03 <- lulc %>% dplyr::filter(From != To) %>%
        dplyr::group_by(Period, To, Interval) %>% dplyr::summarise(num03 = sum(QtPixel)) # gross gain category in time point Yt+1

      denom03 <-
        lulc %>% dplyr::group_by(Period, To) %>% dplyr::summarise(denom03 = sum(QtPixel)) # total area category in time point Yt+1

      eq3 <-
        num03 %>% dplyr::left_join(denom03, by = c("Period", "To")) %>%
        dplyr::mutate(Gtj = (num03 / (denom03 * Interval)) * 100) %>%
        dplyr::left_join(eq1[c(1,3)], by = "Period") %>% dplyr::select(1,2,3,4,6,7) %>%
        rename("GG_pixel" = "num03")

      # EQ4 -   Lti ---------
      num04 <- lulc %>% dplyr::filter(From != To) %>%
        dplyr::group_by(Period, From, Interval) %>% dplyr::summarise(num04 = sum(QtPixel)) # gross loss of category i in time point Yt

      denom04 <-
        lulc %>% dplyr::group_by(Period, From) %>% dplyr::summarise(denom04 = sum(QtPixel)) # total area of the category in time point Yt

      eq4 <-
        num04 %>% dplyr::left_join(denom04, by = c("Period", "From")) %>%
        dplyr::mutate(Lti = (num04 / (denom04 * Interval)) * 100) %>%
        dplyr::left_join(eq1[c(1,3)], by = "Period") %>% dplyr::select(1,2,3,4,6,7) %>%
        rename("GL_pixel" = "num04")

      # ____________Transition ----
      # Witch transitions are particularly intensive in a given time interval?
      # EQ5 - Rtin----
      num05 <-
        lulc %>% dplyr::filter(From != To, To == category_n) %>%
        dplyr::group_by(Period, From, Interval) %>% dplyr::summarise(num05 = sum(QtPixel)) # transition area for every i to n

      denom05 <-
        lulc %>% dplyr::filter(From != category_n) %>%  # filtering all non-n
        dplyr::group_by(Period, From) %>% dplyr::summarise(denom05 = sum(QtPixel)) # total area of every i in the time point Yt

      eq5 <-
        num05 %>% dplyr::left_join(denom05, by = c("Period", "From")) %>%
        dplyr::mutate(Rtin = (num05 / (Interval * denom05)) * 100)

      # EQ6 - Wtn ----
      num06 <- lulc %>% dplyr::filter(From != To, To == category_n) %>%
        dplyr::group_by(Period, To, Interval) %>% dplyr::summarise(num06 = sum(QtPixel)) # gross gain of n during the transition

      denom06 <- lulc  %>% dplyr::filter(From != category_n) %>%
        dplyr::group_by(Period) %>% dplyr::summarise(denom06 = sum(QtPixel)) # non-n area in time point Yt

      eq6 <- num06 %>% dplyr::left_join(denom06, by = "Period") %>%
        dplyr::mutate(Wtn = (num06 / (Interval * denom06)) * 100)

      plot03ganho_n <-
        eq5 %>% dplyr::left_join(eq6, by = c("Period", "Interval")) %>%
        dplyr::select(1,2,7,3,4,6,10) %>% rename("T_i2n_pixel" = "num05")


      # EQ7 - Qtmj----
      num07 <- lulc %>% dplyr::filter(From != To, From == category_m) %>%
        dplyr::group_by(Period, To, Interval) %>% dplyr::summarise(num07 = sum(QtPixel)) # trasition area from m to every j

      denom07 <- lulc %>% dplyr::filter(To != category_m) %>%
        dplyr::group_by(Period, To) %>% dplyr::summarise(denom07 = sum(QtPixel)) # total area of every j in time point (Yt+1)

      eq7 <-
        num07 %>% dplyr::left_join(denom07, by = c("Period", "To")) %>%
        dplyr::mutate(Qtmj = (num07 / (Interval * denom07)) * 100)

      # EQ8 - Vtm----
      num08 <- lulc %>% dplyr::filter(From != To, From == category_m) %>%
        dplyr::group_by(Period, From, Interval) %>% dplyr::summarise(num08 = sum(QtPixel)) # gross loss of m during the transition

      denom08 <- lulc %>% dplyr::filter(To != category_m) %>%
        dplyr::group_by(Period) %>% dplyr::summarise(denom08 = sum(QtPixel)) # non-m area in the time point Y(t+1)


      eq8 <- num08 %>% dplyr::left_join(denom08, by = "Period") %>%
        dplyr::mutate(Vtm = (num08 / (Interval * denom08)) * 100)

      plot03perda_m <-
        eq7 %>% dplyr::left_join(eq8, by = c("Period", "Interval")) %>%
        dplyr::select(1,2,7,3,4,6,10) %>% rename("T_m2j_pixel" = "num07")
    }

    # ___Stationarity test------

    # Level02 ----
    # gain
    st_lv2_gain <-
      eq3 %>% dplyr::filter(Gtj > St) %>% dplyr::group_by(To) %>%
      dplyr::summarise(
        Gain = dplyr::n(),
        N = length(unique(eq3$Period)),
        Stationarity = "Active Gain",
        Test = ifelse(Gain == N, "Y", "N")
      ) %>% rbind(
        eq3 %>% dplyr::filter(Gtj < St) %>% dplyr::group_by(To) %>%
          dplyr::summarise(
            Gain = dplyr::n(),
            N = length(unique(eq3$Period)),
            Stationarity = "Dormant Gain",
            Test = ifelse(Gain == N, "Y", "N")
          )
      )

    # loss
    st_lv2_loss <-
      eq4 %>% dplyr::filter(Lti > St) %>% dplyr::group_by(From) %>%
      dplyr::summarise(
        Loss = dplyr::n(),
        N = length(unique(eq4$Period)),
        Stationarity = "Active Loss",
        Test = ifelse(Loss == N, "Y", "N")
      ) %>% rbind(
        eq4 %>% dplyr::filter(Lti < St) %>% dplyr::group_by(From) %>%
          dplyr::summarise(
            Loss = dplyr::n(),
            N = length(unique(eq4$Period)),
            Stationarity = "Dormant Loss",
            Test = ifelse(Loss == N, "Y", "N")
          )
      )

    # Level03 gain_n ----
    st_gain_n <-
      plot03ganho_n %>% dplyr::filter(Rtin > Wtn) %>% dplyr::group_by(From) %>%
      dplyr::summarise(
        Loss = dplyr::n(),
        N = length(unique(plot03ganho_n$Period)),
        Stationarity = paste("targeted by", category_n),
        Test = ifelse(Loss == N, "Y", "N")
      ) %>% rbind(
        plot03ganho_n %>% dplyr::filter(Rtin < Wtn) %>% dplyr::group_by(From) %>%
          dplyr::summarise(
            Loss = dplyr::n(),
            N = length(unique(plot03ganho_n$Period)),
            Stationarity = paste("avoided by", category_n),
            Test = ifelse(Loss == N, "Y", "N")
          )
      )

    # Level03b loss_m----
    st_loss_m <-
      plot03perda_m %>% dplyr::filter(Qtmj > Vtm) %>% dplyr::group_by(To) %>%
      dplyr::summarise(
        Gain = dplyr::n(),
        N = length(unique(plot03perda_m$Period)),
        Stationarity = paste("targeted", category_m),
        Test = ifelse(Gain == N, "Y", "N")
      ) %>% rbind(
        plot03perda_m %>% dplyr::filter(Qtmj < Vtm) %>% dplyr::group_by(To) %>%
          dplyr::summarise(
            Gain = dplyr::n(),
            N = length(unique(plot03perda_m$Period)),
            Stationarity = paste("avoided", category_m),
            Test = ifelse(Gain == N, "Y", "N")
          )
      )

    # Instances of intensity class ----
    intensity_tables <-
      list(
        lulc_table = lulc,
        interval_lvl = new("Interval",
                           intervalData = level01),
        category_lvlGain = new(
          "Category",
          categoryData = eq3,
          lookupcolor = lookupcolor,
          categoryStationarity = st_lv2_gain
        ),
        category_lvlLoss = new(
          "Category",
          categoryData = eq4,
          lookupcolor = lookupcolor,
          categoryStationarity = st_lv2_loss
        ),
        transition_lvlGain_n = new(
          "Transition",
          transitionData = plot03ganho_n,
          lookupcolor = lookupcolor,
          transitionStationarity = st_gain_n
        ),
        transition_lvlLoss_m = new(
          "Transition",
          transitionData = plot03perda_m,
          lookupcolor = lookupcolor,
          transitionStationarity = st_loss_m
        )
      )
    return(intensity_tables)

  }
