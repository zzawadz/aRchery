#' Plot total scores by day
#'
#' @param data data.frame containing archery data.
#' @param ... other parameters passed to \code{geom_line}.
#' 
#' @export
#' 
#' @examples 
#' 
#' data(archeryData)
#' plot_daily_total_points(archeryData)
#' 
plot_daily_total_points <- function(
  data,
  addRollMean = TRUE,
  addRollMax = TRUE,
  n = 7, 
  ...) {
  
  dt <- seplyr::group_by_se(data, groupingVars = "Date")
  dt <-  seplyr::summarise_se(
    dt,
    setNames("sum(as.numeric(RawScore))", "TotalScore")
  )
  
  dt <- seplyr::arrange_se(dt, "Date")
  dt[[paste0("Mean", n)]] <- TTR::runMean(dt[["TotalScore"]], n = n)
  dt[["Max"]] <- TTR::runMax(dt[["TotalScore"]], cumulative = TRUE, n = 1)
  
  dt <- reshape2::melt(dt, id = "Date", na.rm = TRUE)  
  
  pl <- ggplot2::ggplot(dt) + 
    ggplot2::geom_line(
      ggplot2::aes_string(
        "Date", "value",
        color = "variable",
        linetype = "variable"),
      ...
    )
  pl
}


plot_medians_polygon <- function(data) {
  
  splData <- aRchery::split_by(data, "Date")
  x <- splData[["data"]][[1]]
  
  get_median <- function(x) {
    x[, "y"] <- -x[, "y"]
    
    median <-
      DepthProc::depthMedian(
        x[, c("x", "y")], 
        depth_params = list(method = "Tukey", exact = TRUE)
      )
    median
  }
  
  medians <- t(simplify2array(lapply(splData[["data"]], get_median)))
  plot_target()
  
  colors <- gray.colors(nrow(medians), start = 0, end = 1)
  
  for(i in head(seq_len(nrow(medians)), -1)){
    segments(
      medians[i,1], medians[i,2],
      medians[i+1,1], medians[i+1,2],
      col = colors[i], lwd = 2
    )
  }
}
