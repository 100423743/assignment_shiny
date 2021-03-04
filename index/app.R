#
# This is a Shiny web application
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinyjs)
library(Stat2Data)

data(HousesNY)
data <- HousesNY

list_choices <- colnames(data[,2:5])
model_price <- lm(Price ~ Beds+Baths+Size+Lot, data=data)

# UI
ui <- navbarPage("Shiny app",
                 tabPanel("Grapher",
                          fluidPage(
                              sidebarLayout(sidebarPanel(
                                  selectInput("select", label = h3("House price by feature"), 
                                              choices = list_choices,
                                              selected = 1)
                              ), mainPanel(
                                  h3("Plots"),
                                  plotOutput(outputId = "plots", click = "plot_click")
                              )
                              ))),
                 tabPanel("Price calculator",
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                            fluidRow(
                                                h3(style = "margin-left: 0px; margin-bottom: 0px;", "Number of Beds"),
                                                column(12,
                                                       sliderInput("n_beds", label="", min = 2, max = 6, value = 2, step = 1)
                                                )
                                            ),
                                            fluidRow(
                                                h3(style = "margin-left: 0px; margin-bottom: 0px;", "Number of Baths"),
                                                column(12,
                                                       sliderInput("n_baths", label="", min = 1.0, max = 3.5, value = 1.0, step = 0.5)
                                                )
                                            ),
                                            fluidRow(
                                                h3(style = "margin-left: 0px; margin-bottom: 0px;", "Size (in 1,000 square feet)"),
                                                column(12,
                                                       sliderInput("n_size", label="", min = 0.712, max = 3.100, value = 0.712, step = 0.001)
                                                )
                                            ),
                                            fluidRow(
                                                h3(style = "margin-left: 0px; margin-bottom: 0px;", "Lot (in acres)"),
                                                column(12,
                                                       sliderInput("n_lot", label="", min = 0.00, max = 3.50, value = 0.00, step = 0.01)
                                                )
                                            ),
                                        ),
                                        mainPanel(
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Calculator", textOutput("Calculator")),
                                                        tabPanel("Linear Model", verbatimTextOutput("Summary")),
                                                        tabPanel("Table", tableOutput("Table"))
                                            )
                                        ))
                 ),
                 useShinyjs()
)

# SERVER
server <- function(input, output) {
    output$plots <- renderPlot({
        ggplot(data, aes(.data[[input$select]], Price, colour = as.character(.data[[input$select]]))) +
            geom_point() + theme(legend.position="none")
    })
    
    #############################
    output$Calculator <- renderPrint({
        price <- summary(model_price)$coef[1,1] + summary(model_price)$coef[2,1]*input$n_beds +
            summary(model_price)$coef[3,1]*input$n_baths + summary(model_price)$coef[4,1]*input$n_size +
            summary(model_price)$coef[5,1]*input$n_lot
        
        cat("Estimated price for a house with:", input$n_beds, "beds,", input$n_baths,
            "baths,", input$n_size*1000, "square feet and", input$n_lot,
            "acres would be:", price, "$")})
    
    
    output$Summary <- renderPrint(summary(model_price))
    output$Table <- renderTable(data)
}

# Run the application 
shinyApp(ui = ui, server = server)