library(shiny)
library(shinyWidgets)
library(DT)
library(bslib)
library(markdown)



# Themes and Dark Mode
light <<- bs_theme(bootswatch = "flatly")
dark <<- bs_theme(bg = "black", fg = "white", primary = "purple")


#Function for rendering the Input panels of both players
renderInputs <- function(prefix) {
    wellPanel(
        fluidRow(column(12,
            radioGroupButtons(paste0(prefix, "_", "policy"), label = h3("Scheduling Policies"),
                         choices = list("Block Scheduling" = 2, "Open Scheduling" = 1, "Mixed Block Scheduling" = 3), 
                         selected = 2, individual = TRUE,
                         checkIcon = list(
                             yes = tags$i(class = "fa fa-check-square", 
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-square-o", 
                                         style = "color: steelblue")
                             ) 
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
        column(6, numericInput("ward_capacity","The Bed Capacity in the Ward: ",
                               value = 7,
                               min = 1,
                               max = 200,
                               step = 10)
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
                            value = 240,
                            min = 120,
                            max = 1000,
                            step = 50)
               )
        # column(4,
        #        numericInput("warmup_period","Set the warmup period of the simulation:",
        #                     value = 0,
        #                     min = 0,
        #                     max = 400,
        #                     step = 50)
        # )
        )
        # column(6,
        #        awesomeCheckbox(
        #            inputId = "random_seed_value",
        #            label = "Shall the seed value be set randomly ?", 
        #            value = TRUE,
        #            status = "danger"
        #        ))
        )
        
}

renderOutputs <- function(prefix) {
    wellPanel(
        fluidRow(column(12, tags$h3("Performance Measures"))),
        hr(),
        fluidRow(
            column(6,tags$h4("Access Time"),
                   br(),
                   tableOutput(paste0(prefix, "_", "pAccessTime"))),
            column(6,tags$h4("Utilizaton of the OR's"),
                   br(),
                   tableOutput( paste0(prefix, "_", "utilizationOR")))),
        fluidRow(column(12,tags$h4("Bed Shortages"),
                        br(),
                        tableOutput(paste0(prefix, "_", "bedShortage")))),
        
        fluidRow(column(12, tags$h3("Explanatory Measures"))),
        hr(), 
        fluidRow(column(12,tags$h4("Waiting List Length"),
                        br(),
                        plotOutput( paste0(prefix, "_", "lengthOfWaitingList")))),
        br(),
        fluidRow(column(12,tags$h4("Bed Occupancy at the Ward"),
                        br(),
                        plotOutput( paste0(prefix, "_", "bedOccupancy")))),
        br(),
        fluidRow(column(12,tags$h4("Idle time per Sugical Speciality"),
                        br(),
                        plotOutput( paste0(prefix, "_", "idleTime"))))
    )
    
}


# Define UI for application that plots random distributions
navbarPage(title = "Navigation Bar",theme = light, 
           tabPanel("Players Page",
                    fluidPage(
                              tags$h1("OR Appointment Scheduling"),
                              fluidRow(
                                  column(10, 
                                         p("This is a serious game about the appointment scheduling of elective patients. The game shows off the effects different scheduling policys like 'Open', 'Block' and 'Modified Block' can have on the performance of the Operating Room, the access times of patients, and the Ward.
                                           Play this game with two players and see who can create the best schedule! If you are only one Player explore the different policys and schedules side by side!")
                                         ),
                                  column(2, offset = 10, 
                                         switchInput(
                                             inputId = "notifications_on",
                                             label = "Notifications", 
                                             labelWidth = "80px",
                                             value = TRUE
                                             )
                                         )
                              ),
                             
                              hr(),
                              # Main Body
            
                            fluidRow(
                                column(6, tags$h2("Player 1")),
                                column(6, tags$h2("Player 2"))
                            ),
                            fluidRow(
                                column(6, tags$h3("Input Parameters")),
                                column(6, tags$h3("Input Parameters"))
                            ),
                            fluidRow(
                                column(6, renderInputs("a")),
                                column(6, renderInputs("b"))
                            ),
                            hr(),
                            fluidRow(
                                column(4, offset = 4,
                                       actionBttn(
                                           inputId = "run",
                                            label = "Run the Simulation",
                                            style = "jelly", 
                                            color = "success",
                                           icon = icon("running"),
                                           size = "lg",
                                           block = TRUE
                                        )
                                )
                            ),
                            hr(),
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
                        fluidRow(column(6,tags$h3("Arrival Rates")), column(6, tags$h3("Service Rates"))),
                        hr(),
                        wellPanel(
                            render_operator_inputs_type("a"),
                            render_operator_inputs_type("b"),
                            render_operator_inputs_type("c")
                        ),
                        fluidRow(
                            column(6,tags$h3("Capacity of Hospital")), 
                            column(6, tags$h3("General Settings"))
                            ),
                        hr(),
                       
                        fluidRow(
                            column(6,render_operator_inputs_capacity()),
                            column(6,render_operator_inputs_general())
                            ), 
                        flowLayout(
                            column(12, 
                                tags$h3("Appearance"),
                                br(),
                                materialSwitch("dark_mode", "Dark mode", status = "primary")
                                )
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
           tabPanel("Manual",
                    fluidRow(
                        column(6, offset = 3,
                               includeMarkdown("text/Manual.md"))
                    )),
           tabPanel("About",
                    fluidRow(
                        column(6, offset = 3,
                               includeMarkdown("text/About.md"))
                    )))
