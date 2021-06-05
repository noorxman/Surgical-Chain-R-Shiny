##################### OVERSTEER SIMULATION ############################

library(shiny)
library(shinyWidgets)
library(simmer)
library(simmer.plot)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Oversteer Test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("run", "Run the simulation for 100 units "),
            
            sliderTextInput("doc1",
                        "Where should doctor 1 be ?",
                        choices = c("OC", "OR"),
                        selected = "OR"),
                        
            sliderTextInput("doc2",
                            "Where should doctor 1 be ?",
                            choices = c("OC", "OR"),
                            selected = "OC"),
            
            sliderTextInput("doc3",
                            "Where should doctor 1 be ?",
                            choices = c("OC", "OR"),
                            selected = "OC"),

            ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("usage")
        )
    )
)

# Define server logic required to draw outputs
server <- function(input, output, session) {
    
    #Constants 
    
    num_doc <- 3
### REACTIVE PART ###

    #always call reactive funcitons with reactiveFunctionName()
    #reactive change of the state of the system based on input changes
    #logic of capacity changes 
    
    oc_capacity <- reactive({
        (input$doc1 == "OC") + 
        (input$doc2 == "OC") + 
        (input$doc3 == "OC")
    })
    
    # Alert and reset the slider if more than one doc is selected for OR 
    observe({
             if(( (input$doc1 == "OR") + (input$doc2 == "OR") + (input$doc3 == "OR")) > 1) {
                 sendSweetAlert(session,
                                title = "INVALID INPUT",
                                text = "Only one doctor can be in the operating 
                                room at the same time!",
                                type = "error")
                 
                 updateSliderTextInput(
                     session = session,
                     inputId = "doc1",
                     selected = "OC"
                 )
                 
                 updateSliderTextInput(
                     session = session,
                     inputId = "doc2",
                     selected = "OC"
                 )
                 
                 updateSliderTextInput(
                     session = session,
                     inputId = "doc3",
                     selected = "OC"
                 )
             }
        })
    
    
    or_capacity <- reactive({
        (input$doc1 == "OR") + 
        (input$doc2 == "OR") + 
        (input$doc3 == "OR")
    })
    
    
### FIXED PART ###

        # Initialize the simulation at the beginning
        oversteer <- simmer("oversteer")
        
        # Define trajectories to follow
        
        normal <- trajectory("normal") %>%
            
            # Set capacity dynamically based on reactive functions 
            set_capacity("oc", function() {oc_capacity()} ) %>% 
            set_capacity("or", function() {or_capacity()} ) %>% 
            
            # Follow the surgical chain
            
            seize("oc") %>%
            timeout(10) %>% 
            release("oc") %>% 
            
            
            seize("or") %>% 
            timeout(10) %>%
            release("or") 
        
        # Build the environment with resources and generators
    
        oversteer %>%
            add_resource('oc', capacity = 0) %>%
            add_resource("or", capacity = 0) %>%
            add_generator("patient", normal, function() {10})
        

### RUN THE SIMULATION WITHOUT RESET ### 
        
        #important to save the sim enviorement into a reactive sim variable
        #that is called like a function in the output part
        
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
