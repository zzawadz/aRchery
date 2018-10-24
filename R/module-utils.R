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

#' @rdname archeryMiscellaneousPlotsServer
#' @export
archeryMiscellaneousPlotsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinydashboard::box(size = 6, title = "Daily plot", plotOutput(
      ns("DailyPlot"), width = "100%", height = 600
    )),
    shinydashboard::box(size = 6, title = "Scale curve", plotOutput(
      ns("ScaleCurve"), width = "100%", height = 600
    )),
    shinydashboard::box(
      size = 6,
      title = "Daily shot series",
      selectInput(
        ns("NSeries"),
        label = "Number of shots in group",
        choices = c(1,  2,  3,  4,  5,  6, 10, 12, 15, 20, 30),
        selected = 12
      ),
      plotOutput(ns("FncBoxPlot"), width = "100%", height = 600)
    ),
    shinydashboard::box(size = 6, title = "Daily median shot", plotOutput(
      ns("DailyMedians"), width = "100%", height = 600
    ))
  )
}


#' archeryMiscellaneousPlotsServer
#'
#' @param input 
#' @param output 
#' @param session 
#' @param mainData 
#' @param selectedDates 
#'
#' @rdname archeryMiscellaneousPlotsServer
#' @export
#' 
archeryMiscellaneousPlotsServer <- function(input, output, session, mainData, selectedDates) {
  
  output$ScaleCurve <- renderPlot({
    req(mainData(), selectedDates())
    dtAll <- mainData()[as.character(mainData()[["Date"]]) %in% selectedDates(), ]
    aRchery::plot_scale_curves(dtAll, "Date")
  })
  
  output$DailyPlot <- renderPlot({
    req(mainData())
    aRchery::plot_daily_total_points(mainData(), size = 2)
  })
  
  output$FncBoxPlot <- renderPlot({
    req(mainData())
    
    dtMatrix12 <- fnc_by_n_shots(mainData(), as.numeric(input$NSeries))
    plot_fnc_boxplot(dtMatrix12)
  })
  
  output$DailyMedians <- renderPlot({
    req(mainData())
    aRchery::plot_medians_polygon(mainData())
  })
}