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
    ),
    shinydashboard::box(
      width = 12,
      shiny::plotOutput(ns("CalendarPlot"))
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
  
  output$DataInfo <- renderPrint({
    cat(paste("Days:", length(unique(dataAll()$Date))), "\n")
    cat(paste("Interval:", paste(range(dataAll()[["Date"]]), collapse = " - ")), "\n")
    cat(paste("Shots No:", nrow(dataAll())), "\n")
  })
  
  output$CalendarPlot <- renderPlot({
    req(dataAll())
    Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
    x <- dataAll() %>% 
      group_by(Date) %>% 
      summarise(Score = sum(RawScore))
    openair::calendarPlot(
      x %>% rename(date = Date),
      pollutant = "Score",
      par.settings = list(fontsize=list(text=25))
    )
  })
  
  return(dataAll)
}