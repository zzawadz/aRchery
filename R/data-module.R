#' @rdname archeryDataServer
#' @export
archeryDataUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shinydashboard::box(
      width = 6,
      fileInput(ns("FilePath"), label = "Upload file", multiple = TRUE),
      actionButton(ns("DefaultData"), label = "Default data", width = "100%")
    ),
    shinydashboard::box(
      width = 6,
      verbatimTextOutput(ns("DataInfo"))
    )
  )
  
}

#' Module for loading archery data
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @rdname archeryDataServer
#' @export
#'
archeryDataServer <- function(input, output, session) {
  
  dataContainer <- reactiveValues(data = NULL)
  
  observeEvent(input$DefaultData, {
    dataContainer$data <- archeryData
  })
  
  observeEvent(input$FilePath, {
    req(input$FilePath)
    data <- read_archery_files(input$FilePath$datapath)
    dataContainer$data <- data
  })
  
  dataAll <- reactive({
    req(dataContainer$data)
    dataContainer$data
  })
  
  
  return(dataAll)
}