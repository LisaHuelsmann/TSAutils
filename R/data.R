


#' Weather data from Hurdal, Norway
#'
#' A dataset with weather data recorded in Hurdal, Norway.
#'
#' @format A data frame with 17520 rows and 4 variables:
#' \describe{
#'   \item{weather_Hurdal}{date, POSIXct}
#'   \item{Precipitation (mm)}{Numeric observed value}
#'   \item{Tair@42m (deg C)}{Numeric observed value}
#'   \item{Tair@2m (deg C)}{Numeric observed value}
#' }
#' @source Holger Lange
"weather_Hurdal"


#' Global monthly mean atmospheric CO2 concentration
#'
#' A monthly time series of global mean atmospheric carbon dioxide (CO2)
#' concentration values at sea level, expressed in parts per million (ppm).
#' The dataset spans from 1979 to 2025 and is indexed by fractional year.
#'
#' @format A data frame with 564 rows and 2 variables:
#' \describe{
#'   \item{time}{Numeric. Time expressed as fractional year
#'   (for example, 1979.042 for approximately January 1979).}
#'   \item{CO2}{Numeric. Global monthly mean atmospheric CO2 concentration,
#'   in parts per million (ppm).}
#' }
#'
#' @details
#' The dataset contains monthly observations of global atmospheric CO2
#' concentration.
#'
#' @source Obtained from https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_gl.txt.
"CO2_global"
