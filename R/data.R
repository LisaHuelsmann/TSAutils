


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


#' Daily meteorological data
#'
#' Daily meteorological observations including temperature, vapor pressure
#' deficit, wind speed, global radiation, precipitation, relative humidity,
#' and evapotranspiration at the DWD weather station Schulenburg in the Harz
#' mountains in Germany.
#'
#' @format A data frame with 25,915 rows and 12 variables:
#' \describe{
#'   \item{Date}{Date of observation.}
#'   \item{Year_Month}{Year-month identifier.}
#'   \item{dec_year}{Decimal year.}
#'   \item{Tmean}{Daily mean air temperature at 2 m height (°C).}
#'   \item{Tmin}{Daily minimum air temperature at 2 m height (°C).}
#'   \item{Tmax}{Daily maximum air temperature at 2 m height (°C).}
#'   \item{VPD}{Vapor pressure deficit (kPa).}
#'   \item{Wind}{Daily wind speed (m s^-1).}
#'   \item{Rg}{Global radiation. Unit to be confirmed.}
#'   \item{P}{Daily precipitation sum. Unit to be confirmed, likely mm.}
#'   \item{RH}{Relative humidity. Unit to be confirmed, likely percent.}
#'   \item{ET}{Evapotranspiration. Unit to be confirmed, likely mm day^-1.}
#' }
#'
#' @details
#' The dataset contains daily meteorological observations over approximately
#' 71 years. The measurements were recorded at the DWD weather station
#' Schulenburg in the Harz mountains in Germany.
#'
#' @source
#' Deutscher Wetterdienst DWD
#'
#' @examples
#' data(meteo_Harz)
#' head(meteo_Harz)
#' summary(meteo_Harz)
"meteo_Harz"
