#' Plot total scores by day
#'
#' @param data data.frame containing archery data.
#'
#' @export
#'
plot_daily_total_points <- function(
  data,
  addRollMean = TRUE,
  addRollMax = TRUE,
  n = 7) {
  
  dt <- seplyr::group_by_se(data, groupingVars = "Date")
  dt <-  seplyr::summarise_se(
    dt,
    setNames("sum(as.numeric(RawScore))", "TotalScore")
  )
  
  dt <- arrange_se(dt, "Date")
  dt[[paste0("Mean", n)]] <- TTR::runMean(dt[["TotalScore"]], n = n)
  dt[["Max"]] <- TTR::runMax(dt[["TotalScore"]], cumulative = TRUE, n = 1)
  
  dt <- reshape2::melt(dt, id = "Date", na.rm = TRUE)  
  
  pl <- ggplot2::ggplot(dt) + 
    ggplot2::geom_line(
      ggplot2::aes_string("Date", "value", color = "variable")
    )
  pl
}
