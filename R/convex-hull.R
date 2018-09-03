add_convex_hull <- function(data, col = "black") {
  x <- data[["x"]]
  y <- -data[["y"]]
  
  hpts <- chull(x = x, y = y)
  hpts <- c(hpts, hpts[1])
  lines(x[hpts], y[hpts], col = col, lwd = 1.5)
}

#' Title
#'
#' @param data archery data.
#' @param vars vars to split by.
#'
#' @examples
#' 
plot_convex_hull_by <- function(data, vars) {

  target <- get_target_type(data)
  plot_target(target)
  splData <- split_by(data, vars)
  dtList <- splData[["data"]]
  
  colors <- rev(paste0("grey", floor(seq_along(dtList) / length(dtList) * 100)))
  
  lapply(seq_along(dtList), function(i) {
    add_convex_hull(dtList[[i]], col = colors[i])
  })
  
}
