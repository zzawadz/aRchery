library(shiny)
library(aRchery)
library(dplyr)

options(shiny.maxRequestSize=1024^3)

shinyServer(function(input, output, session) {

    dataAll <- callModule(archeryDataServer, id = "Data")
    selectedDates <- callModule(archeryDateModuleServer, id = "SelectedDate", mainData = dataAll)
    
    observe({
        req(dataAll())
        dates <- unique(dataAll()[["Date"]])
        
        updateSelectInput(
            session = session, inputId = "Date",
            choices = dates, selected = dates[1])
        updateSelectInput(
            session = session, inputId = "GroupSize",
            choices = (1:30)[(60 %% 1:30) == 0], selected = 6)
        
    })
    
    observeEvent(input$GroupSize, {
        req(input$GroupSize)
        updateSliderInput(
            session, inputId = "Slider", min = 1, 
            max = 60 / as.numeric(input$GroupSize), value = 1
        )
    })
    
    data <- reactive({
        req(input$Date, input$GroupSize)
        ar_make_mean_grouped_data(
            data = dataAll() %>% filter(Date == input$Date),
            as.numeric(input$GroupSize))
    })
    
    output$Plot <- renderPlot({
        req(data(), input$Date, input$GroupSize, input$Slider)
        i <- input$Slider
        
        dt <- data()[["summaries"]] %>% head(i)
        
        plot_target_with_shots(dt, cex = input$PointSize)
        ar_add_shots(data()[["rawPoints"]][[i]], colors = "gray", cex = input$PointSize)
    })

})
