library(shiny)
library(shinyWidgets)
library(DT)
library(bslib)



# Themes and Dark Mode
light <<- bs_theme()
dark <<- bs_theme(bg = "black", fg = "white", primary = "purple")


#Function for rendering the Input panels of both players
renderInputs <- function(prefix) {
    wellPanel(
        fluidRow(column(12,
            radioGroupButtons(paste0(prefix, "_", "policy"), label = h3("Scheduling Policies"),
                         choices = list("Block Scheduling" = 2, "Open Scheduling" = 1, "Mixed Block Scheduling" = 3), 
                         selected = 2, individual = TRUE,
                         checkIcon = list(
                             yes = tags$i(class = "fa fa-circle",
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-circle-o",
                                         style = "color: steelblue"))
                         )
                )
        ),
        fluidRow(column(12,
                        DTOutput(paste0(prefix, "_", 'input_schedule'))
                )
            
        )
        
    )
}

render_operator_inputs_type <- function (prefix) {
    
    fluidRow(column(6, sliderInput(paste0("mean_inter_arrival_", prefix),paste0("Mean Inter Arrival Times of Type ", prefix, ":" ),
                                    min = 1,
                                    max = 30,
                                    value = 5)
                    ),
             column(6, sliderInput(paste0("mean_service_rate_", prefix),paste0("Mean Service Rate of Type ", prefix, ":" ),
                                  min = 1,
                                  max = 30,
                                  value = 5)
             )
             
        )
        
}

# render_operator_inputs_service_rate <- function () {
#     wellPanel(
#         fluidRow(
#             column(6, sliderInput("mean_service_rate_a","Mean Service Rate of Type A:",
#                                   min = 1,
#                                   max = 30,
#                                   value = 5)
#             ),
#             column(6, sliderInput("mean_service_rate_b","Mean Service Rate of Type B:",
#                                   min = 1,
#                                   max = 30,
#                                   value = 5)
#             )
#         )
#     )
# }

render_operator_inputs_capacity <- function () {
    wellPanel(
    fluidRow(
        column(6, pickerInput(
        inputId = "number_or",
        label = "Number of Operating Rooms: ", 
        choices = c("2", "3", "4", "5"),
        options = list(
            style = "btn-primary")
    )
    ))
            
    )
        
}
    
render_operator_inputs_general <- function () {
    wellPanel(
    fluidRow(
        column(4, 
               numericInput("seed_value","Set the seed value for the generation of simulated random values:",
                                    value = 1,
                                    min = 1,
                                    max = 200,
                                    step = 1)
               ), 
        column(4,
               numericInput("run_time","Set the run time of the simulation:",
                            value = 200,
                            min = 100,
                            max = 600,
                            step = 50)
               )
        # column(6,
        #        awesomeCheckbox(
        #            inputId = "random_seed_value",
        #            label = "Shall the seed value be set randomly ?", 
        #            value = TRUE,
        #            status = "danger"
        #        ))
        )
            
    )
        
}

renderOutputs <- function(prefix) {
    wellPanel(
        fluidRow(column(12, tags$h3("Performance Measures"))),
        fluidRow(column(12,tableOutput( paste0(prefix, "_", "pAccessTime")))),
        fluidRow(column(12,tableOutput( paste0(prefix, "_", "utilizationOR")))),
        fluidRow(column(12,tableOutput( paste0(prefix, "_", "bedShortage")))),
        fluidRow(column(12, tags$h3("Explanatory Measures"))),
        fluidRow(column(12,plotOutput( paste0(prefix, "_", "lengthOfWaitingList")))),
        fluidRow(column(12,plotOutput( paste0(prefix, "_", "bedOccupancy")))),
        fluidRow(column(12,plotOutput( paste0(prefix, "_", "idleTime"))))
    )
    
}


# Define UI for application that plots random distributions
navbarPage(title = "Navigation Bar",theme = light, 
           tabPanel("Players Page",
                    fluidPage(
                        # theme="simplex.min.css",
                        #       tags$style(type="text/css",
                        #                  "label {font-size: 12px;}",
                        #                  ".recalculating {opacity: 1.0;}"),
                        #       
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
                                            label = "Run the Simulation",
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
           tabPanel("Operators Page",
                    fluidPage(
                        tags$h1("OR Appointment Scheduling"),
                        p("Here you can change the inputs of the simulation to create a new challenging enviroment!"),
                        hr(),
                        fluidRow(column(6,tags$h2("Arrival Rates")), column(6, tags$h2("Service Rates"))),
                        wellPanel(
                            render_operator_inputs_type("a"),
                            render_operator_inputs_type("b")
                        ),
                        fluidRow(
                            column(6,tags$h2("Capacity of Hospital")), 
                            column(6, tags$h2("General Settings"))
                            ),
                       
                        fluidRow(
                            column(6,render_operator_inputs_capacity()),
                            column(6,render_operator_inputs_general())
                            ), 
                        flowLayout(
                            column(4, materialSwitch("dark_mode", "Dark mode", status = "primary"))
                        )
                        
                        
                        
                        # fluidRow(column(4, offset = 4, tags$h2("General"))),
                        # render_operator_inputs_general(),
                        # fluidRow(column(4, offset = 4, tags$h2("Arrivals"))),
                        # render_operator_inputs_arrival_rate(),
                        # fluidRow(column(4, offset = 4, tags$h2("Service"))),
                        # render_operator_inputs_service_rate(),
                        # fluidRow(column(4, offset = 4, tags$h2("Capacity"))),
                        # render_operator_inputs_capacity()
                        
                        )
                    ),
           tabPanel("Manual"),
           tabPanel("About"))
