##################### OVERSTEER SIMULATION ############################

library(shiny)
library(shinyWidgets)
library(simmer)
library(simmer.plot)

set.seed(2021)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Oversteer Test"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("run", "Run the simulation for 100 units "),
            actionBttn("reset", "Reset the simulation", style = "material-flat"),
            numericInput("runtime",
                         "How long should the simulation run at a time ?",
                         value = 100),
            
            sliderTextInput("doc1",
                        "Where should doctor A be ?",
                        choices = c("OC", "OR"),
                        selected = "OC"),
                        
            sliderTextInput("doc2",
                            "Where should doctor B be ?",
                            choices = c("OC", "OR"),
                            selected = "OC"),
            
            sliderTextInput("doc3",
                            "Where should doctor C be ?",
                            choices = c("OC", "OR"),
                            selected = "OC"),

            ),
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(6, plotOutput("oc_usage")),
                column(6, plotOutput("or_usage")),
            ),
            plotOutput("util"),
            dataTableOutput("resources")
                       
            
           
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
    
    # oc_capacity <- reactive({
    #     (input$doc1 == "OC") + 
    #     (input$doc2 == "OC") + 
    #     (input$doc3 == "OC")
    # })
    
    oc_a_capacity <- reactive({
        (input$doc1 == "OC") + 0
        # as.numeric(input$doc1 == "OC")
    })
    
    oc_b_capacity <- reactive({
        (input$doc2 == "OC") + 0
    })
    
    oc_c_capacity <- reactive({
        (input$doc3 == "OC") + 0
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
    #set the reactive functions that are called to change capacity
    
    or_a_capacity <- reactive({
        (input$doc1 == "OR") + 0
    })
    
    or_b_capacity <- reactive({
        (input$doc2 == "OR") + 0
    })
    
    or_c_capacity <- reactive({
        (input$doc3 == "OR") + 0
    })
    
    # or_capacity <- reactive({
    #     (input$doc1 == "OR") + 
    #     (input$doc2 == "OR") + 
    #     (input$doc3 == "OR")
    # })
    
    
### FIXED PART ###

        # Initialize the simulation at the beginning
        oversteer <- simmer("oversteer")
        
        # Define trajectories to follow
        
        normal_a<- trajectory("normal_a") %>%
            
            # Set capacity dynamically based on reactive functions 
            set_capacity("oc_a", function() {oc_a_capacity()} ) %>% 
            
            
            # Follow the surgical chain
            
            seize("oc_a") %>%
            timeout(10) %>% 
            release("oc_a") %>% 
            
            # Set capacity dynamically based on reactive functions 
                #note: capactiy only changes if some patient is going through 
                #this trajectory
            set_capacity("or_a", function() {or_a_capacity()} ) %>%
            
            seize("or_a") %>% 
            timeout(10) %>%
            release("or_a")
        
        normal_b <- trajectory("normal_b") %>%
            
            # Set capacity dynamically based on reactive functions 
            set_capacity("oc_b", function() {oc_b_capacity()} ) %>% 
            
            
            # Follow the surgical chain
            
            seize("oc_b") %>%
            timeout(10) %>% 
            release("oc_b") %>% 
            
            set_capacity("or_b", function() {or_b_capacity()} ) %>%
            
            seize("or_b") %>% 
            timeout(10) %>%
            release("or_b") 
        
        normal_c <- trajectory("normal_c") %>%
            
            # Set capacity dynamically based on reactive functions 
            set_capacity("oc_c", function() {oc_c_capacity()} ) %>% 
            
            
            # Follow the surgical chain
            
            seize("oc_c") %>%
            timeout(10) %>% 
            release("oc_c") %>% 
            
            set_capacity("or_c", function() {or_c_capacity()} ) %>% 
            
            seize("or_c") %>% 
            timeout(10) %>%
            release("or_c") 
        
        
        # Build the environment with resources and generators
    
        oversteer %>%
            add_resource("oc_a", capacity = 0) %>%
            add_resource("oc_b", capacity = 0) %>%
            add_resource("oc_c", capacity = 0) %>%
            add_resource("or_a", capacity = 0) %>%
            add_resource("or_b", capacity = 0) %>%
            add_resource("or_c", capacity = 0) %>%
            add_generator("patient_a", normal_a, function() {10}) %>%
            add_generator("patient_b", normal_b, function() {10}) %>%
            add_generator("patient_c", normal_c, function() {10})
        

### RUN THE SIMULATION WITHOUT RESET ### 
        
        #important to save the sim enviorement into a reactive sim variable
        #that is called like a function in the output part
        
    sim <- eventReactive(input$run, {
        
        oversteer %>% run(until = now(oversteer) + input$runtime)
    })
    # Reset the simulation and notify the user
    
    observeEvent(input$reset, {
        oversteer %>% reset()
        sendSweetAlert(session,
                       title = "Reset was sucesfull!",
                       text = "Run the simulation again and it will start over",
                       type = "success")
        
    })
    
### RENDER THE PLOTS BASED ON THE SIMULATION ###

    output$or_usage <- renderPlot({
        # ggplot(data = subset(x =get_mon_resources(sim()), resource == "oc_a"),mapping = aes(x = time, y = queue)) +
        #     geom_col(mapping = aes(x = time, y = queue_size)) + 
        #     labs(title = "QUEUE LENGTHS OVER TIME", x = "TIME", y = "QUEUE LENGTH",
        #          caption = "Only for outpatient clinic a")
        # #plot(get_mon_resources(sim()))
        
        #ggplot(data = get_mon_resources(sim()), mapping = aes(time, queue)) + 
           # geom_line(aes(color = factor(resource)))
        
        or_pl <- subset(get_mon_resources(sim()), resource == c("or_a", "or_b", "or_c"))
        
        ggplot(data = or_pl, mapping = aes(time, queue)) + 
            geom_line(aes(color = factor(resource))) +
            labs(title = "QUEUE in OR")
            #facet_grid(cols =  vars(c("or_a", "or_b", "or_c")))
    })
    
    output$resources <- renderDataTable(get_mon_resources(sim()))
    
    output$oc_usage <- renderPlot({
        # get the required data     
        resource_stat <- get_mon_resources(sim())
        oc_capacity_stat <- data.frame(time = vector(), queue = vector() )
        oc_pl <- subset(resource_stat, resource == c("oc_a", "oc_b", "oc_c"))
        
        # extract the queue length for all the oc's together 
        for(i in oc_pl[, "time"]) {
            queue_size <- sum(oc_pl[oc_pl$time == i,  "queue"])
            oc_capacity_stat <- rbind(oc_capacity_stat, data.frame(time = i, queue = queue_size))
        }
        oc_capacity_stat <- unique(oc_capacity_stat)
        
        # plot the queue length for each patient type and for all together
        ggplot(data = NULL, mapping = aes(time, queue)) + 
            geom_line(data = oc_pl, aes(color = factor(resource))) +
            geom_line(data = oc_capacity_stat, aes(x = time, y = queue)) +
            labs(title = "QUEUE in OC")
        #facet_grid(cols =  vars(c("or_a", "or_b", "or_c")))
        
    })
    output$util <- renderPlot({
        plot(get_mon_resources(sim()), metric = "utilization")
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
