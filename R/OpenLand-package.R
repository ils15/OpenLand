"_PACKAGE"
#' OpenLand: Quantitative Analysis and Visualization of Land Use and Cover Change
#'
#' OpenLand is a comprehensive R package for analyzing land use and cover (LUC) 
#' time series data. It provides tools for consistency checking, loading spatiotemporal 
#' raster data, and creating synthesized spatial visualizations. The package implements 
#' multiple LUC change (LUCC) metrics for regular or irregular time intervals, with 
#' visualization through one- and multistep sankey and chord diagrams.
#'
#' @section Core Functionality:
#' The package implements a complete intensity analysis framework following 
#' Aldwaik and Pontius (2012), with tools for generating standardized multilevel 
#' output graphics and comprehensive change detection algorithms.
#'
#' @section Performance Optimizations (v1.1.0):
#' \describe{
#'   \item{Enhanced Core Functions}{Vectorized operations and optimized algorithms in intensityAnalysis}
#'   \item{Advanced Memory Management}{Dynamic chunk sizing based on available system memory}
#'   \item{Terra Integration}{Optional terra package support for 2-3x faster raster processing}
#'   \item{Parallel Processing}{Multi-core support for large dataset analysis with future.apply}
#'   \item{Memory Efficiency}{Improved memory management for large raster time series}
#'   \item{Backward Compatibility}{Full compatibility maintained with raster package}
#'   \item{Chunked Processing}{Handle datasets larger than available RAM with intelligent optimization}
#'   \item{Performance Monitoring}{Built-in benchmarking and optimization status reporting}
#' }
#'
#' \strong{Key Functions:}
#' \itemize{
#'   \item \code{\link{summary_dir}}: Optimized directory summary with terra support
#'   \item \code{\link{summary_map}}: Enhanced pixel counting with vectorized operations
#'   \item \code{\link{contingencyTable}}: Land use change analysis with class exclusion options
#'   \item \code{\link{intensityAnalysis}}: Enhanced performance with vectorized operations
#'   \item \code{\link{performance_status}}: Check performance optimization status
#'   \item \code{\link{benchmark_performance}}: Comprehensive performance benchmarking
#' }
#'
#' \strong{New Features in Version 1.1.0:}
#' \itemize{
#'   \item Enhanced vectorized operations in \code{intensityAnalysis} for faster computation
#'   \item Dynamic memory optimization with intelligent chunk sizing
#'   \item Performance benchmarking suite for measuring optimization gains
#'   \item Advanced parallel processing with future.apply integration
#'   \item Comprehensive performance monitoring and optimization recommendations
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
