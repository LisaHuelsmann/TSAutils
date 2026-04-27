


#' Plot monthly boxplots
#'
#' Creates boxplots of a numeric variable grouped by month. The function expects
#' the input data to contain a `Year_Month` column formatted as `"YYYY_MM"` or
#' similar, where the month is the second element after splitting by `"_"`.
#'
#' @param data A data frame containing a `Year_Month` column.
#' @param x A numeric vector with the same length as the number of rows in
#'   `data`. This variable is plotted as monthly boxplots.
#'
#' @return No return value. The function is called for its side effect of
#'   creating a base R boxplot.
#'
#' @details
#' Monthly groups are extracted from `data$Year_Month`. The boxplot labels use
#' the full English month names from `month.name`.
#'
#' @examples
#' month_boxplot(meteodata, meteodata$Tmean)
#'
#' @export
month_boxplot <- function(data, x) {
  dummy <- strsplit(as.character(data$Year_Month), split = "_")
  month_year <- do.call(rbind, lapply(dummy, "["))
  boxplot(x ~ month_year[, 2], names = month.name)
}
