


#' Plot seasonal boxplot
#' @export
season.box = function(data, x) {
  dummy = strsplit(as.character(data$Year_Month), split = "_")
  month_year = do.call(rbind, lapply(dummy, "["))
  boxplot(x ~ month_year[, 2], names = month.name)
}
