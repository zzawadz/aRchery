library(shiny)
library(shinydashboard)

MENU <- sidebarMenu(
    menuItem("Grouping Mean", tabName = "GroupingMean")
)

BODY <- tabItems(
    tabItem(tabName = "GroupingMean",
        shinydashboard::box(title = "Menu", width = 4,
            selectInput("Date", label = "Select date:", choices = NULL, selected = NULL),
            selectInput("Group size", inputId = "GroupSize", choices = NULL, selected = NULL),
            sliderInput("Slider", label = "Slider", min = 0, max = 12, value = 1)
        ),
        shinydashboard::box(title = "Menu", width = 8,
            plotOutput("Plot", width = "100%", height = "70vh")
        )
    )
)


dashboardPage(
    dashboardHeader(title = "reseaRcher"),
    dashboardSidebar(MENU),
    dashboardBody(BODY)
)