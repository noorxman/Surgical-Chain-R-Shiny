library(shiny)
library(simmer)
library(simmer.plot)

ui <- fluidPage(
    
    # Application title
    titlePanel("First Shiny Simulation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("inter_arrival_times",
                        "Inter Arrival Times:",
                        min = 1,
                        max = 50,
                        value = 20),
            checkboxInput("inter_arrival_times_var",
                          "Should the arrival times be variable ?"),
            
            sliderInput("service_times",
                        "Service Times for all stations:",
                        min = 1,
                        max = 50,
                        value = 20),
            checkboxInput("service_times_var",
                          "Should the service times be variable ?"),
            
            tags$h3("Capacity"),
            
            sliderInput("cap_oc",
                        "Capacity of Outpatient Clinic:",
                        min = 1,
                        max = 10,
                        value = 1),
            sliderInput("cap_or",
                        "Capacity of Operating Room",
                        min = 1,
                        max = 10,
                        value = 1),
            sliderInput("cap_ward",
                        "Capacity of Ward",
                        min = 1,
                        max = 10,
                        value = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel( tabsetPanel(
            tabPanel("TAB1", plotOutput("usage")),
            tabPanel("TAB2", plotOutput("util"))
        )
        
        )
    )
    
    
)



server <- function(input,output) {
    # save outputs to output$hist in order to add to the output list
    # use the render functions to create objects that can be passes as outputs
    # acess input values with input$num for example 
    
    output$usage <- renderPlot(
        { #curly brackets used for code blocks
            #simulating a simple health center (Surgical chain edition)
            set.seed(2021)
            
            
            
            ### SOME CONSTANTS
            arrival_rate <- function() {  #Arrival rate either static or stochastic
                if(input$inter_arrival_times_var == TRUE) {
                    rexp(1,1/input$inter_arrival_times)
                } else {
                    input$inter_arrival_times
                }
            }
            ##Capacity
            cap_oc <- input$cap_oc
            cap_or <- input$cap_or
            cap_wd <- input$cap_ward
            ##Service Times
            serv_oc <-
                if(input$service_times_var == TRUE) {
                    rexp(1,1/input$service_times)
                } else {
                    input$service_times
                }
            
            serv_or <- serv_oc
            serv_wd <- serv_oc
            
            ### SIMULATION ENVIROMENT
            
            patient <- trajectory("patients path") %>%
                ## add nurse intake activity
                seize("outpatient_clinic", amount = 1) %>%
                timeout(serv_oc) %>%
                release("outpatient_clinic") %>%
                
                ## add consultation activity with doc
                seize("operating_room",amount = 1) %>%
                timeout(serv_or) %>%
                release("operating_room", amount = 1) %>%
                
                ##add a planning activity
                seize("ward") %>%
                timeout(serv_wd) %>%
                release("ward")
            
            #for replications use: hospital <- lapply(1:100,function(){ INSERT SIMULATION ENVIOREMENT})
            
            
            hospital <- simmer("hospital") %>%
                add_resource("outpatient_clinic",cap_oc) %>%
                add_resource("operating_room",cap_or) %>%
                add_resource("ward", cap_wd) %>%
                add_generator("patient", patient,arrival_rate) %>%
                run(until = 480)
            
            plot(get_mon_resources(hospital), metric = "usage", items = "queue")
            
        }
    )
    
    
}

shinyApp(ui = ui, server = server)