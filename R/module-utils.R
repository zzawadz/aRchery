#' @export
#' @rdname archeryDateModuleServer
archeryDateModuleUI <- function(id) {
  ns <- NS(id)
  list(
    selectInput(
      ns("Dates"),
      label = "Selected dates", choices = NULL, multiple = TRUE)
  )
}

#' Module for selecting dates
#'
#' @param input 
#' @param output 
#' @param session 
#' @param mainData 
#'
#' @export
#' @rdname archeryDateModuleServer
#' 
archeryDateModuleServer <- function(input, output, session, mainData) {
  
  observe({
    req(mainData())
    choices <- sort(unique(mainData()[["Date"]]))
    selected <-
      na.omit(unique(c(
        choices[1], 
        choices[length(choices) / 2], 
        choices[length(choices)]))
      )
    updateSelectInput("Dates",
                      session = session,
                      choices = choices,
                      selected = selected)
  })
  
  selectedDates <- reactive({
    input$Dates
  })
  
  return(selectedDates)
  
}