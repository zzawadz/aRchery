#' @export
ARCHERY_TARGETS <- list(
  WA5_RING_40 = list(
    rads = c(20, 16, 12, 8, 4, 2),
    colors = c("blue", "red", "red", "yellow", "yellow", "yellow")
  )
)


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
  
  rads <- target$rads
  colors <- target$colors
  rads <- rads / max(rads)
  
  plot(0,0, xlim = c(-1,1)*1.2, ylim = c(-1,1) * 1.2, axes = FALSE, xlab = "", ylab = "", type = "n")
  mapply(
    function(x, col) plotrix::draw.circle(0,0,x, col = col),
    rads, colors
  )
  return(invisible())
}
