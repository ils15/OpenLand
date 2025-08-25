"_PACKAGE"
#' OpenLand: land use and cover (LUC) time series analysis in R.
#'
#' OpenLand is an open-source R package for the analysis of land use and cover
#' (LUC) time series. It includes support for consistency check and loading
#' spatiotemporal raster data and synthesized spatial plotting. Several LUC change
#' (LUCC) metrics in regular or irregular time intervals can be extracted and
#' visualized through one- and multistep sankey and chord diagrams. A complete
#' intensity analysis according to \cite{(Aldwaik and Pontius, 2012)} is
#' implemented, including tools for the generation of standardized multilevel
#' output graphics.
#'
#' \strong{Performance Improvements in v1.0.3.9000+:}
#' \itemize{
#'   \item Enhanced performance through optional terra package integration
#'   \item 2-10x faster raster processing for large datasets
#'   \item Improved memory management for large raster time series
#'   \item Backward compatibility maintained with raster package
#'   \item Vectorized operations replace manual loops for better performance
#' }
#'
#' \strong{Key Functions:}
#' \itemize{
#'   \item \code{\link{summary_dir}}: Optimized directory summary with terra support
#'   \item \code{\link{summary_map}}: Enhanced pixel counting with vectorized operations
#'   \item \code{\link{contingencyTable}}: Land use change analysis with class exclusion options
#'   \item \code{\link{performance_status}}: Check performance optimization status
#' }
#'
#' \strong{New Features in Latest Version:}
#' \itemize{
#'   \item Class exclusion support in \code{contingencyTable} (exclude_classes parameter)
#'   \item Remove background or no-data classes from land use change analysis
#'   \item Improved workflow for handling multi-class raster datasets
#' }
#'
#' @seealso The core functions in this package: \code{\link{intensityAnalysis}},
#' \code{\link{contingencyTable}}, \code{\link{performance_status}}
#'
#' @author Reginal Exavier \email{reginalexavier@@rocketmail.com}, Peter Zeilhofer \email{zeilhoferpeter@@gmail.com}
# @docType package
#' @name OpenLand-package
#' @aliases OpenLand
#'
#'
#'
#' @references
#' Aldwaik, S. Z. and Pontius, R. G. (2012) ‘Intensity analysis to unify
#' measurements of size and stationarity of land changes by interval, category,
#' and transition, Landscape and Urban Planning. Elsevier B.V., 106(1), pp. 103–114.
#' \doi{10.1016/j.landurbplan.2012.02.010}.
#'
#'
#'
#'
#'
NULL
