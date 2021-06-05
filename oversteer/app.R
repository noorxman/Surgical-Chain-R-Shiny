##################### OVERSTEER SIMULATION ############################

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
            actionButton("run", "Run the simulation for 100 units "),
            
            sliderInput("oc_doc1",
                        "Is doctor 1 you have in the OC ?",
                        min = 0,
                        max = 1,
                        value = 1,
                        step = 1),
        sliderInput("oc_doc2",
                    "Is the doctor 2 you have in the OC ?",
                    min = 0,
                    max = 1,
                    value = 1,
                    step = 1),
    sliderInput("oc_doc3",
                "Is the doctor 3 you have in the OC ?",
                min = 0,
                max = 1,
                value = 1,
                step = 1),
),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("usage")
        )
    )
)

# Define server logic required to draw outputs
server <- function(input, output) {
    
    #Constants 
    
    num_doc <- 3
    ### REACTIVE PART ###
#always call reactive funcitons with reactiveFunctionName()
    # Reactive change of the state of the system based on input changes
    #logic of capacity changes 
    
    oc_capacity <- reactive({
        input$oc_doc1 + 
        input$oc_doc2 + 
        input$oc_doc3
    })
    
    or_capacity <- reactive({
        1 - input$oc_doc1 +
        1 - input$oc_doc2 +
        1 - input$oc_doc3
    })
    
    
    ### FIXED PART ###
        #fixed code that is run once to build the simulation
        oversteer <- simmer("oversteer")
        
        
        normal <- trajectory("normal") %>%
            
            # Set capacity dynamically based on reactive functions 
            set_capacity("oc", function() {oc_capacity()} ) %>% 
            set_capacity("or", function() {or_capacity()} ) %>% 
            
            seize("oc") %>%
            timeout(10) %>% 
            release("oc") %>% 
            
            
            seize("or") %>% 
            timeout(10) %>%
            release("or") 
    
        oversteer %>%
            add_resource('oc', capacity = 1) %>%
            add_resource("or", capacity = 0) %>%
            add_generator("patient", normal, function() {10})
        

    ### RUN THE SIMULATION BASED WITHOUT RESET ### 
        
        #important to save the sim enviorement into and reactive sim variable
        #that is called like a function
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
