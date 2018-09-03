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
#' # set graphical parameters
#' plot_target(setPars = TRUE)
#' 
plot_target <- function(target = ARCHERY_TARGETS$WA5_RING_40, setPars = FALSE) {
  
  if(setPars) {
    oldPars <- par(no.readonly = TRUE, xpd = NA)
    par(mar = c(1,1,1,1))
    on.exit({
      par(oldPars)      
    })
  }
  
  rads <- target$rads
  colors <- target$colors
  rads <- rads / max(rads)
  
  plot(0,0, xlim = c(-1,1)*1, ylim = c(-1,1) * 1, axes = FALSE, xlab = "", ylab = "", type = "n")
  mapply(
    function(x, col) plotrix::draw.circle(0, 0, x, col = col),
    rads, colors
  )
  return(invisible())
}

#' Plot target with shots
#'
#' @param data archery data
#'
#' @export
#'
#' @examples
#' 
#' data(archeryData)
#' 
#' dates <- unique(archeryData$Date)
#' dates <- dates[c(1, length(dates))]
#' firstDay <- archeryData[archeryData[["Date"]] == dates[1],]
#' lastDay <- archeryData[archeryData[["Date"]] == dates[2],]
#' 
#' oldPar <- par(
#'    no.readonly = TRUE, mfrow = c(1,2),
#'    mar = c(1,1,1,1), xpd = NA)
#'    
#' plot_target_with_shots(firstDay)
#' title(dates[1])
#' plot_target_with_shots(lastDay)
#' title(dates[2])
#' 
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
