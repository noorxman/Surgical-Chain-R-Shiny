#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    init_schedule <- data.frame("Mon" = "OR","Tue" = "OC","Wed" = "OC",
                                "Thu" = "OC", "Fri" = "OR" )
    
    output$input_schedule = renderDT(
        init_schedule, editable = "cell", options = list(dom = 't') , 
        autoHideNavigation = TRUE, class = "cell-border stripe"
    )
    
    observeEvent(input$input_schedule_cell_edit, {
        init_schedule <<- editData(sch,input$input_schedule_cell_edit)
        print(init_schedule)
    })
    

})
