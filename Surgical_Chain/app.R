############### SURGICAL CHAIN SIMULATION ################################

library(shiny)
library(simmer)
library(simmer.plot)
library(bslib)

ui <- fluidPage( theme = bs_theme(version = 4,bootswatch = "flatly"),
    
    # Application title
    titlePanel("First Shiny Simulation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("arrival_rate", "Average patients ariving per hour: ",
                        min = 1,
                        max = 50,
                        value = 10),
            checkboxInput("arrival_rate_var", "Should the arrival rates be variable ?"),
            
            sliderInput("service_rates", "Average patients served per hour:",
                        min = 1,
                        max = 50,
                        value = 10),
            checkboxInput("service_rates_var", "Should the service rates be variable ?"),
            
            tags$h3("Capacity"),
            
            sliderInput("cap_oc", "Capacity of Outpatient Clinic:",
                        min = 1,
                        max = 10,
                        value = 1),
            sliderInput("cap_or", "Capacity of Operating Room",
                        min = 1,
                        max = 10,
                        value = 1),
            sliderInput("cap_ward", "Capacity of Ward",
                        min = 1,
                        max = 10,
                        value = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tags$h3("Conceputal Representation"),
            img(src = "Surgical_Chain_Revisit.png", height = 250), #possible to do  height  = "30%", width ="70%"
            
            tabsetPanel(
                tabPanel("Usage", plotOutput("usage")),
                tabPanel("Utilziation", plotOutput("util")),
                tabPanel("Activity Time", plotOutput("activ")),
                tabPanel("Waiting Time", plotOutput("wait")),
                tabPanel("Flow Time", plotOutput("flow"))
        )
        
        
        )
    )
    
    
)



server <- function(input,output) {
    # save outputs to output$hist in order to add to the output list
    # use the render functions to create objects that can be passes as outputs
    # acess input values with input$num for example 
    
    
    # Modularize the simulation using reactive()
    
    sim <- reactive({
        
        
        
        
        ### SOME CONSTANTS
        arrival_rate <- function() {  #Arrival rate either static or stochastic
            if(input$arrival_rate_var == TRUE) {
                rexp(1,1/input$arrival_rate *60)
            } else {
                1/input$arrival_rate * 60
            }
        }
        ##Capacity
        cap_oc <- input$cap_oc
        cap_or <- input$cap_or
        cap_wd <- input$cap_ward
        ##Service Times
        serv_oc <-
            if(input$service_rates_var == TRUE) {
                rexp(1,1/input$service_rates * 60)
            } else {
                1/input$service_rates * 60
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
            run(until = 100)
        
    })
    
    ####OUTPUTS ######
    
    output$usage <- renderPlot({ 
            plot(get_mon_resources(sim()), metric = "usage", items = "queue",
                 c("outpatient_clinic", "operating_room", "ward"))
        })
    
    output$util <- renderPlot({ 
        plot(get_mon_resources(sim()), metric = "utilization",
             c("outpatient_clinic", "operating_room", "ward"))
    })
    
    output$activ <- renderPlot({ 
        plot(get_mon_arrivals(sim()), metric = "activity_time")
    })
    
    output$wait <- renderPlot({ 
        plot(get_mon_arrivals(sim()), metric = "waiting_time")
    })
    
    output$flow <- renderPlot({ 
        plot(get_mon_arrivals(sim()), metric = "flow_time")
    })
    
    
}

shinyApp(ui = ui, server = server)