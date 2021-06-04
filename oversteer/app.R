#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(simmer)
library(simmer.plot)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Oversteer Test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("run", "Run the simulation"),
            sliderInput("oc_doc",
                        "Is the doctor in the OC ?",
                        min = 0,
                        max = 1,
                        value = 1,
                        step = 1), 
            sliderInput("or_doc",
                        "Doctor is in OR ?",
                        min = 0,
                        max = 1,
                        value = 1,
                        step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("usage")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    state <- reactive({
        if (input$oc_doc == 0) {
            cap <- 1
            1 
        } else {
            cap <- 2
            2
        }
    })
    ### FIXED PART ###
        #fixed code that is run once to build the simulation
        oversteer <- simmer("oversteer")
        
        
        normal <- trajectory("normal") %>%
            log_("I am in normal path") %>%
            seize("oc") %>%
            timeout(10) %>% 
            release("oc") %>% 
            log_("oh boy") %>%
            seize("or") %>% 
            timeout(10) %>%
            release("or")
        patient <- trajectory("patient") %>%
            branch( option = function() {state()}, continue = TRUE,
                    # option 1: CHange the capacity of oc to 0 and or to 1
                    trajectory() %>%
                        log_( function() { paste("The capactiy of oc is 0 state:", state())}) %>%
                        set_capacity("oc", 0 ) %>%
                        set_capacity("or", 1),
                    # option 2: change the capacity of oc back to 1 and or to 0
                    trajectory() %>%
                        log_(function() { paste("The capactiy of oc is 0 state:", state())}) %>%
                        set_capacity("oc", 1) %>% 
                        set_capacity("or", 0)
            ) %>%
            log_("I am joining the normal path") %>%
            join(normal)
    
        oversteer %>%
            add_resource('oc', capacity = 1) %>%
            add_resource("or", capacity = 0) %>%
            add_generator("patient", patient, function() {10})
            
    ### REACTIVE PART ###
    
    #logic of capacity changes 
        
       

    #RUN THE SIMULATION BASED ON THE 
    sim <- eventReactive(input$run, {
        
        oversteer %>% run(until = now(oversteer) + 100)
    })
    ### RENDER THE PLOTS BASED ON THE SIMULATION ###
    output$usage <- renderPlot({
        plot(get_mon_resources(sim()))
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
