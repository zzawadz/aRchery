add_convex_hull <- function(data, col = "black") {
  x <- data[["x"]]
  y <- -data[["y"]]
  
  hpts <- chull(x = x, y = y)
  hpts <- c(hpts, hpts[1])
  lines(x[hpts], y[hpts], col = col, lwd = 1.5)
}

#' Plot target with convex hull.
#'
#' @param data archery data.
#' @param vars vars to split by.
#'
#' @examples
#' 
#' data(archeryData)
#' plot_convex_hull_by(archeryData, "Date")
#' 
plot_convex_hull_by <- function(data, vars) {

  layout(cbind(1,2), widths = c(2,1))

  target <- get_target_type(data)
  plot_target(target)
  splData <- split_by(data, vars)
  dtList <- splData[["data"]]
  
  colors <- rev(paste0("grey", floor(seq_along(dtList) / length(dtList) * 100)))
  
  lapply(seq_along(dtList), function(i) {
    add_convex_hull(dtList[[i]], col = colors[i])
  })
  
  desc <- apply(splData[, -ncol(splData)], 1, function(x)
    paste(unlist(x), collapse = " - "))
  
  par(xpd = TRUE)
  plot(0,0, type = "n", axes = FALSE)
  legend(
    "topleft",
    desc,
    col = colors,
    border = NA,pch = 19
  )    
}
