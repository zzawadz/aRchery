#' Sample archery data
#' @docType data
#' @keywords datasets
#' @name archeryData
#' @usage data(archeryData)
#' @format archery data.
#'
NULL


#' Start an aRchery
#'
#' @param port The TCP port that the application should listen on.
#' @param host The IPv4 address that the application should listen on.
#' @param launch.browser If true, the system's default web browser will be launched automatically after the app is started. 
#' @param ... other parameters passed to \code{\link{runApp}}
#'
#' @export
#' 
run_archery_app <- function(
  port = 5005,
  host = "127.0.0.1",
  launch.browser = FALSE, 
  ...) {
  shiny::runApp(
    appDir = system.file("aRchery", package = "aRchery"),
    port = port,
    host = host,
    launch.browser = launch.browser,
    ...
  )
}

