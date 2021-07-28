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

    ## Creating the appointment schedules for Player 1/A 
    
    a_init_schedule <- data.frame("Mon" = c("Type A", "Type B", "Type C"),
                                  "Tue" = c("Type A", "Type B", "Type C"),
                                  "Wed" = c("Type A", "Type B", "Type C"),
                                  "Thu" = c("Type A", "Type B", "Type C"),
                                  "Fri" = c("Type A", "Type B", "Type C")
                        )
    row.names(a_init_schedule) <- c("OR 1", "OR 2", "OR 3")
    
    output$a_input_schedule = renderDT(
        a_init_schedule,
        editable = "cell",
        autoHideNavigation = TRUE,
        options = list(dom = 't'),
        class = "cell-border stripe"
    )
    
    observeEvent(input$a_input_schedule_cell_edit, {
        a_init_schedule <<- editData(a_init_schedule,input$a_input_schedule_cell_edit)
        print(a_init_schedule)
    })
    
    ## Creating the appointment schedules for Player 1/A 
    b_init_schedule <- data.frame("Mon" = c("Type A", "Type B", "Type C"),
                                  "Tue" = c("Type A", "Type B", "Type C"),
                                  "Wed" = c("Type A", "Type B", "Type C"),
                                  "Thu" = c("Type A", "Type B", "Type C"),
                                  "Fri" = c("Type A", "Type B", "Type C")
    )
    row.names(b_init_schedule) <- c("OR 1", "OR 2", "OR 3")
    
    output$b_input_schedule = renderDT(
        b_init_schedule,
        editable = "cell",
        autoHideNavigation = TRUE,
        options = list(dom = 't'),
        class = "cell-border stripe"
    )
    
    observeEvent(input$b_input_schedule_cell_edit, {
        b_init_schedule <<- editData(b_init_schedule,input$b_input_schedule_cell_edit)
        print(b_init_schedule)
    })
    # ## Assigning the outputs to be rendered in the UI 
    # 
    # #Text/Measure Outputs
    # output$a_pAccessTime
    # output$b_pAccessTime
    # 
    # output$a_utilizationOR
    # output$b_utilizationOR
    # 
    # output$a_bedShortage
    # output$b_bedShortage
    # #Plot Outputs 
    # 
    # output$a_lengthOfWaitingList
    # output$b_lengthOfWaitingList
    # 
    # 
    # output$a_bedOccupancy
    # output$b_bedOccupancy
    # 
    # 
    # output$a_idleTime
    # output$b_idleTime
    # 
})
