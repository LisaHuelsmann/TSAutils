


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


#' Sensible heat flux at Hurdal in 2024
#'
#' A half-hourly time series of sensible heat flux \eqn{H} measured in 2024
#' using eddy covariance at the tower site in Hurdal.
#'
#' The sensible heat flux was calculated from vertical wind velocity,
#' air temperature, and air density. Positive values indicate a downward
#' heat flux, meaning that the system gains heat.
#'
#' @format A data frame with 17,568 rows and 1 variable:
#' \describe{
#'   \item{H}{Sensible heat flux in \eqn{W m^{-2}}. Positive values indicate
#'   downward heat flux.}
#' }
#'
#' @details
#' The data have a temporal resolution of 30 minutes and cover the full year 2024
#' (including leap year).
#'
#' @source
#' Eddy covariance measurements from the tower in Hurdal.
#'
#'
#' @examples
#' data(heatflux_Hurdal)
#' head(heatflux_Hurdal)
#' plot(heatflux_Hurdal$H, type = "l",
#'      ylab = expression(H~"(W"~m^{-2}~")"),
#'      main = "Sensible heat flux at Hurdal, 2024")
"heatflux_Hurdal"


#' Synthetic data for Fourier analysis
#'
#' A synthetic univariate time series used to illustrate discrete Fourier
#' decomposition and reconstruction.
#'
#' @format A data frame with 30 rows and 2 variables:
#' \describe{
#'   \item{t}{Time index, from 1 to 30.}
#'   \item{x}{Observed synthetic time series value.}
#' }
#'
#' @details
#' The series contains a small number of periodic components and is intended for
#' teaching the interpretation of Fourier coefficients, dominant frequencies,
#' and partial reconstruction from selected Fourier terms.
#'
#' @examples
#' data(fourier_synthetic)
#' plot(fourier_synthetic, type = "b", main = "Synthetic Data")
#'
#' @source Simulated data for teaching purposes.
"fourier_synthetic"



#' Draupner wave height time series
#'
#' A univariate time series of wave-height measurements from the Draupner
#' platform in the North Sea.
#'
#' The object is stored as a [`ts`] time-series object.
#'
#' @format A time series object of class [`ts`].
#' \describe{
#'   \item{Values}{Wave-height measurements, in metres if stored in the original Draupner units.}
#' }
#'
#' @source Draupner platform wave-height record.
#'
#' @examples
#' data(waveheight_Draupner)
#' plot(waveheight_Draupner)
#'
#' @docType data
#' @keywords datasets
#' @name waveheight_Draupner
"waveheight_Draupner"



#' Theoretical entropy-complexity bounds
#'
#' Theoretical lower and upper bounds of the permutation entropy-complexity
#' plane for ordinal patterns with embedding dimensions \eqn{D = 4} and
#' \eqn{D = 5}.
#'
#' These datasets are used as reference curves when plotting permutation
#' entropy against permutation complexity.
#'
#' @format Each object is a data frame with two variables:
#' \describe{
#'   \item{PE}{Normalized permutation entropy.}
#'   \item{MPR}{Permutation complexity.}
#' }
#'
#' @details
#' The four objects contain separate boundary curves:
#' \describe{
#'   \item{entropy_complexity_min_D4}{Lower bound for embedding dimension \eqn{D = 4}.}
#'   \item{entropy_complexity_max_D4}{Upper bound for embedding dimension \eqn{D = 4}.}
#'   \item{entropy_complexity_min_D5}{Lower bound for embedding dimension \eqn{D = 5}.}
#'   \item{entropy_complexity_max_D5}{Upper bound for embedding dimension \eqn{D = 5}.}
#' }
#'
#' The lower and upper curves are stored separately because they are not
#' necessarily evaluated at the same permutation entropy values.
#'
#' @examples
#' data(entropy_complexity_min_D5)
#' data(entropy_complexity_max_D5)
#'
#' plot(entropy_complexity_min_D5$PE,
#'      entropy_complexity_min_D5$MPR,
#'      type = "l",
#'      xlab = "Permutation entropy",
#'      ylab = "Permutation complexity")
#'
#' lines(entropy_complexity_max_D5$PE,
#'       entropy_complexity_max_D5$MPR)
#'
#' @family Time Series Analysis course datasets
#' @keywords datasets
#'
#' @name entropy_complexity_limits
#' @rdname entropy_complexity_limits
#' @aliases entropy_complexity_min_D4
#' @aliases entropy_complexity_max_D4
#' @aliases entropy_complexity_min_D5
#' @aliases entropy_complexity_max_D5
NULL


