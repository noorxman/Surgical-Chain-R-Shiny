library(shiny)
library(tuicalendr)
library(shinyWidgets)
library(simmer.plot)
library(DT)



renderInputs <- function(prefix) {
    wellPanel(
        fluidRow(
            column(6,DTOutput('input_schedule')
                   ),
            column(6,DTOutput('input_schedule')
                   )
        ),
        p(actionButton(paste0(prefix, "_", "recalc"),
                       "Re-run simulation", icon("random")
        ))
    )
}

# Define UI for application that plots random distributions
fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),
          
          # Application title
          tags$h2("OR Appointment Scheduling"),
          p("A serious game about the appointment scheduling of elective patients.
            Play this game with two players and see who can handle the challenges of the Operating Room better!
            "),
          hr(),
          
          fluidRow(
              column(6, tags$h3("Player 1")),
              column(6, tags$h3("Player 2"))
          ),
          fluidRow(
              column(6, renderInputs("a")),
              column(6, renderInputs("b"))
          ),
          fluidRow(
              column(6,
                     plotOutput("a_distPlot", height = "600px")
              ),
              column(6,
                     plotOutput("b_distPlot", height = "600px")
              )
          )
)