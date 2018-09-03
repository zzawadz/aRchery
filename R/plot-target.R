#' @export
ARCHERY_TARGETS <- list(
  WA5_RING_40 = list(
    rads = c(20, 16, 12, 8, 4, 2),
    colors = c("blue", "red", "red", "yellow", "yellow", "yellow")
  )
)

ARCHERY_MAP <- c(
  "WA 5 Ring (40cm)" = "WA5_RING_40"
)

scale_rads <- function(target) {
  x <- target[["rads"]]
  x / max(x)
}

#' Plot target
#'
#' @param target a target object
#'
#' @export
#'
#' @examples
#' 
#' plot_target()
#' 
plot_target <- function(target = ARCHERY_TARGETS$WA5_RING_40) {
  
  oldPars <- par(no.readonly = TRUE)
  par(mar = c(1,1,1,1))
  rads <- target$rads
  colors <- target$colors
  rads <- rads / max(rads)
  
  plot(0,0, xlim = c(-1,1)*1, ylim = c(-1,1) * 1, axes = FALSE, xlab = "", ylab = "", type = "n")
  mapply(
    function(x, col) plotrix::draw.circle(0, 0, x, col = col),
    rads, colors
  )
  
  #par(oldPars)
  return(invisible())
}

plot_target_with_shots <- function(data) {
  
  target <- get_target_type(data)
  
  plot_target(target)
  
  x <- data[["x"]]
  y <- -data[["y"]]
  rad <- sqrt(x^2 + y^2)
  
  scr <- scale_rads(target)
  colors <- vapply(rad, function(x) sum(x > scr), 0) + 1
  points(x = x, y = y, pch = 19, col = colors)
  
}
