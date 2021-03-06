library(shiny)
library(shinydashboard)
library(magrittr)
library(aRchery)

MENU <- sidebarMenu(
    menuItem("Intro", tabName = "Intro"),
    menuItem("Grouping Mean", tabName = "GroupingMean"),
    menuItem("Compare targets", tabName = "CompareTargets"),
    menuItem("Miscellaneous plots", tabName = "MiscPlots"),
    archeryDateModuleUI("SelectedDate")
)

BODY <- tabItems(
    tabItem(tabName = "Intro",
            archeryDataUI("Data")
    ),
    tabItem(tabName = "GroupingMean",
        shinydashboard::box(title = "Menu", width = 4,
            selectInput("Date", label = "Select date:", choices = NULL, selected = NULL),
            selectInput("Group size", inputId = "GroupSize", choices = NULL, selected = NULL),
            sliderInput("Slider", label = "Slider", min = 0, max = 12, value = 1),
            hr(),
            sliderInput("Point size", inputId = "PointSize", min = 1, max = 6, value = 2, step = 0.1)
        ),
        shinydashboard::box(title = "Menu", width = 8, height = "75vh",
            plotOutput("Plot", width = "100%", height = "70vh")
        )
    ),
    tabItem("CompareTargets", archeryCompareTargetsModuleUI("TargetCompare")),
    tabItem("MiscPlots", archeryMiscellaneousPlotsUI("MiscPlots"))
)


dashboardPage(
    dashboardHeader(title = "reseaRcher"),
    dashboardSidebar(MENU),
    dashboardBody(BODY)
)
