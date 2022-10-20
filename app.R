#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(shinyjs)
library(shinydisconnect)

options(shiny.autoreload = TRUE)

data <- read_tsv("rewards.tsv") %>%
    mutate(`$ per point` = Price / Points) # %>%
    # rename(`Price ($)` = Price)

# Define UI for application that draws a histogram
# jscode <- "shinyjs.refresh = function() { history.go(0); }"
ui <- fluidPage(
    # shinydisconnect::disconnectMessage2(),
    # shinyjs::useShinyjs(),
    # shinyjs::extendShinyjs(text = jscode, functions = c("refresh")),
    
    # Application title
    titlePanel("McDonald's Rewards"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("points",
                        "Points to Redeem:",
                        min = 2000,
                        max = 14000,
                        value = c(2000, 10000),
                        step = 2000),
            checkboxGroupInput("meals", label = "Filter By:", choices = c("Breakfast", "Lunch", "Desserts", "Drinks"), selected = c("Breakfast", "Lunch"))#,
            # actionButton("refresh", "Reset", icon = icon("refresh"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # observeEvent(input$refresh, {
    #     shinyjs::js$refresh()
    # })
    
    filtered <- reactive({
        filtered <- data
        if (!is.null(input$meals)) {
            filtered <- filtered %>%
                filter(Meal %in% input$meals)
        }
        
        if(!is.null(input$points[1]) && !is.null(input$points[2])) {
            filtered <- filtered %>%
                filter(Points >= input$points[1],
                       Points <= input$points[2])
        }
        filtered <- arrange(filtered, desc(`$ per point`)) %>%
            mutate(`$ per point` = format(round(`$ per point`, 5), nsmall = 2),
                   Price = str_c("$", Price),
                   Points = format(Points, big.mark = ","))
        filtered
    })
    
    output$table <- renderDataTable({
        filtered()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)