library(shiny)
library(tuicalendr)
library(shinyWidgets)
library(simmer.plot)
library(DT)

#Function for rendering the Input panels of both players
renderInputs <- function(prefix) {
    wellPanel(
        fluidRow(column(12,
            radioButtons(paste0(prefix, "_", "schedulePolicies"), label = h3("Scheduling Policies"),
                         choices = list("Open Scheduling" = 1, "Block Scheduling" = 2, "Mixed Block Scheduling" = 3), 
                         selected = 1),
                )
        ),
        fluidRow(column(12,
                        DTOutput(paste0(prefix, "_", 'input_schedule'))
                )
            
        )
        
    )
}

renderOutputs <- function(prefix) {
    wellPanel(
        fluidRow(column(12, tags$h3("Performance Measures"))),
        fluidRow(column(12,tableOutput( paste0(prefix, "_", "pAccessTime")))),
        fluidRow(column(12,tableOutput( paste0(prefix, "_", "utilizationOR")))),
        fluidRow(column(12,textOutput( paste0(prefix, "_", "bedShortage")))),
        fluidRow(column(12, tags$h3("Explanatory Measures"))),
        fluidRow(column(12,plotOutput( paste0(prefix, "_", "lengthOfWaitingList")))),
        fluidRow(column(12,plotOutput( paste0(prefix, "_", "bedOccupancy")))),
        fluidRow(column(12,plotOutput( paste0(prefix, "_", "idleTime"))))
    )
    
}

# Define UI for application that plots random distributions
navbarPage(title = "Navigation Bar",
           tabPanel("Players Page",
                    fluidPage(theme="simplex.min.css",
                              tags$style(type="text/css",
                                         "label {font-size: 12px;}",
                                         ".recalculating {opacity: 1.0;}"),
                              
                              # Application title
                              tags$h1("OR Appointment Scheduling"),
                              p("A serious game about the appointment scheduling of elective patients. Play this game with two players and see who can handle the challenges of the Operating Room better!"),
                              hr(),
                              # Main Body
            
                            fluidRow(
                                column(6, tags$h2("Player 1")),
                                column(6, tags$h2("Player 2"))
                            ),
                            fluidRow(
                                column(6, tags$h3("Inputs")),
                                column(6, tags$h3("Inputs"))
                            ),
                            fluidRow(
                                column(6, renderInputs("a")),
                                column(6, renderInputs("b"))
                            ),
                            fluidRow(
                                column(4, offset = 4,
                                       actionBttn(
                                           inputId = "run",
                                            label = "run the Simulation",
                                            style = "float", 
                                            color = "success",
                                           icon = icon("running"), 
                                           size = "lg",
                                           block = TRUE
                                        )
                                )
                            ),
                            fluidRow(
                                column(6, tags$h3("Outputs")),
                                column(6, tags$h3("Outputs"))
                            ),
                            fluidRow(
                                column(6,
                                       renderOutputs("a")
                                ),
                                column(6,
                                       renderOutputs("b")
                                )
                            )
                    )
            ),
           tabPanel("Operators Page"),
           tabPanel("Manual"),
           tabPanel("About"))
