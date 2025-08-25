# OpenLand (development version)

## New Features

* `contingencyTable()` now supports flexible naming conventions for input rasters:
  * Added `name_separator` parameter to use different separators (default: "_")
  * Added `year_position` parameter to specify year position ("first", "last", or numeric)
  * Added `name_pattern` parameter for custom regex patterns
  * Maintains full backward compatibility with existing naming convention

## Improvements

* Enhanced robustness of flexible naming features:
  * Automatic detection and handling of R's name modifications (hyphens → dots, numeric prefix → "X" prefix)
  * Intelligent error messages with helpful suggestions for common issues
  * Automatic separator adjustment with informative warnings
  * Better documentation about R's automatic name changes

## Dependencies

* Added `stringr` package dependency for improved string processing

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

