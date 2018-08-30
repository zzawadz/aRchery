#' Plot total scores by day
#'
#' @param data data.frame containing archery data.
#'
#' @return
#' @export
#'
plot_daily_total_points <- function(data) {
  
  dt <- seplyr::group_by_se(data, groupingVars = "Date")
  dt <-  seplyr::summarise_se(
    dt,
    setNames("sum(as.numeric(RawScore))", "TotalScore")
  )
  
  pl <- ggplot2::ggplot(dt) + 
    ggplot2::geom_line(
      ggplot2::aes_string("Date", "TotalScore")
    )
  pl
}
