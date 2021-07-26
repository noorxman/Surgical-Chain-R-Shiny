#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(simmer.plot)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Operating Room Scheduling"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionBttn("run", "RUN", style = "material-flat"),
            actionBttn("reset", "Reset", style = "material-flat"),
            numericInput("runtime",
                         "How long should the simulation run?",
                         value = 100),
            DTOutput('input_schedule')
            
            ),
        # Show a plot of the generated distribution
        mainPanel(fluidRow(
            # column(12, DTOutput('tbl'))
            )
            
            
            # fluidRow(
            #     column(6, plotOutput("oc_usage")),
            #     column(6, plotOutput("or_usage")),
            )
        )
    )
)
