#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(lubridate)
library(stringr)
library(purrr)
library(dplyr)
library(simmer)
library(ggplot2)

#paramNames <- c("init_schedule", "policy", "number_or", "inter_arrival_rate_a",
                # "inter_arrival_rate_b", "inter_arrival_rate_c", "service_rate", "run_time")

varparamNames <- c("policy")

fixparamNames <- c("mean_inter_arrival_a", "mean_inter_arrival_b",
                "mean_service_rate_a", "mean_service_rate_b", "seed_value")


week <- schedule(c(24,48,72,96,120), c(2,3,4,5,1), period = 120)


# Defining a function independent from simulation environment 
get_patient_type_from_schedule_d <- function (or, d, init_schedule) {
    
    room <- as.numeric(str_sub(or,3))
    
    if(init_schedule[room, d] == "Type A") {
        return("a")
    }else if(init_schedule[room, d] == "Type B") {
        return("b")
    }else if(init_schedule[room, d] == "Type C") {
        return("c")
    }else {
        return("open")
    }
    
}

simulate_hospital <- function(init_schedule, policy, mean_inter_arrival_a = 5,
                              mean_inter_arrival_b = 5, mean_inter_arrival_c  = 5,
                              mean_service_rate_a = 5.1, mean_service_rate_b = 5.1,
                              mean_service_rate_c = 5.1,  number_or = 2,
                              seed_value = 1, run_time = 200) {
    #-------------------------------------
    # Inputs
    #-------------------------------------    
    
    init_schedule = init_schedule
    
    # Possion process with exponential inter arrival times 
    inter_arrival_rate_a = 1/mean_inter_arrival_a 
    inter_arrival_rate_b = 1/mean_inter_arrival_b
    inter_arrival_rate_c  = 1/mean_inter_arrival_c
    # 3 Parameter log normal distribuition of service times with mean 5 and
    # shift parameter defined by service rate
    service_rate_a  = mean_service_rate_a - 5
    service_rate_b  = mean_service_rate_b - 5
    service_rate_c  = mean_service_rate_c - 5
    # Policies are created later 
    policy = policy
    
    # General simulation settings
    set.seed(seed_value)
    number_or = number_or
    #-------------------------------------
    # Simulation Functions
    #-------------------------------------  
    
    define_policy <- function(policy) {
        # x <- c()
        if(policy == 1) {
            # for(i in 1:number_or) {
            #     x <- append(x, "open")
            # }
            
            schedule_policy_OR1 <<- "open"
            schedule_policy_OR2 <<- "open"
        } else if (policy == 2) {
            # for(i in 1:number_or) {
            #     x <- append(x, "open")
            # }
            schedule_policy_OR1 <<- "block"
            schedule_policy_OR2 <<- "block"
        } else if (policy == 3) { #& length(which(a_init_schedule[,1] == "Open"))
            # for(i in 1:number_or) {
            #     x <- append(x, "block")
            # }
            # x[1] <- "open"
            
            schedule_policy_OR1 <<- "open"
            schedule_policy_OR2 <<- "block"
            #make the schedule with the modified block asput open in the table instead of Type A (maybe also dependend on the weekday
            # if(which(a_init_schedule[,1] == "Open") == 1) {
            #     schedule_policy_OR1 <<- "open"
            #     schedule_policy_OR2 <<- "block"
            # } else if(which(a_init_schedule[,1] == "Open") == 2) {
            #     schedule_policy_OR1 <<- "block"
            #     schedule_policy_OR2 <<- "open"
            # }
            
        }
        # schedule_policies <<- x
    }
    
    time_to_next_event <- function () {
        x <- peek(hospital, steps = 5, verbose = TRUE)
        x <- x[!x$process == "wait_handler_OR10" & x$time != Inf,]
        x <- x[!x$process == "wait_handler_OR20",]
        if(!is.na(x[1,1])) {
            return(x[1,1]- now(hospital) + 0.001)
        } else {
            return(-1)
            
        }
        
    }
    
    # get_patient_type_from_schedule <- function (or) {
    #     
    #     day <- get_capacity(hospital, "Day_of_week")
    #     room <- as.numeric(str_sub(or,3))
    #     
    #     if(a_init_schedule[[room, day]] == "Type A") {
    #         return("a")
    #     }else if(a_init_schedule[room, day] == "Type B") {
    #         return("b")
    #     }else if(a_init_schedule[room, day] == "Type C") {
    #         return("c")
    #     }
    #     
    # }
    
    selecting_the_next_patient <- function(policy, or) {
        if(policy == "open") {
            selecting_the_next_patient_open(or)
        } else if (policy == "block") {
            selecting_the_next_patient_block(or)
        }
    }
    
    selecting_the_next_patient_open <- function(or) {
        #filter the arrival table to the arrivals in the wating list in ascending order
        x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
        #y <- x[!x$resource == or & !x$resource == "waiting_list",] 
        x <- x[!duplicated(x),]
        # get the entries that are only in the table once = entries that are only in the waiting list
        x <- x[!(duplicated(x$name) | duplicated(x$name, fromLast = TRUE)),]
        x <- x[order(x$start_time),]
        
        if(!is.na(x[1,1])) {
            result <- paste0(x[1,1], " ", or, " ", "Free")
            return(result)
        } else {
            return(paste0("wait_for_patients_",or))
        }
    }
    
    selecting_the_next_patient_block <- function(or) {
        #Extract the number of the current patients
        # patient_name <- get_name(hospital)
        # patient_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", patient_name))
        patient_type <- get_patient_type_from_schedule_d(or,
                                                         get_capacity(hospital, "Day_of_week"),
                                                         init_schedule)
        
        #check if no one of the type is in the schedule 
        
        x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
        x <- x[!duplicated(x),]
        # get the entries that are only in the table once = entries that are only in the waiting list
        x <- x[!(duplicated(x$name) | duplicated(x$name, fromLast = TRUE)),]
        x <- x[order(x$start_time),]
        # filter the patient type
        x <- x[str_detect(str_sub(x$name,9,9), patient_type),]
        
        if(!is.na(x[1,1])) {
            result <- paste0(x[1,1], " ", or, " ", "Free")
            return(result)
        } else {
            return(paste0("wait_for_patients_",or))
        }
        
    }
    
    
    #-------------------------------------
    # Simulation Set Up
    #-------------------------------------  
    
    define_policy(policy)
    
    # Initialize simulation environment
    hospital <- simmer("hospital")
    
    
    # Define Trajectories
    patients_path <- trajectory("patients_path") %>%
        set_global("Entrance", values = 1,mod = "+") %>%
        set_attribute("ID", function() {get_global(hospital, "Entrance")}) %>%
        seize("waiting_list") %>%
        #log_(function() {paste0("Message to be received: ",get_name(hospital), " ", "OR1 Free")}) %>%
        trap(signals = function() {paste0(get_name(hospital), " ", "OR1 Free")},
             handler = trajectory() %>%
                 log_("Message received") %>%
                 release("waiting_list") %>%
                 simmer::select("OR1")%>%
                 log_("GOING OR1"), interruptible = FALSE
        ) %>%
        trap(signals = function() {paste0(get_name(hospital), " ", "OR2 Free")},
             handler = trajectory() %>%
                 log_("Message received") %>%
                 release("waiting_list") %>%
                 simmer::select("OR2") %>%
                 log_("GOING OR2"), interruptible = FALSE
        ) %>%
        log_("WAITING") %>%
        wait() %>% 
        #function(){sample(c("OR1", "OR2"),1)}
        
        seize_selected() %>%
        timeout(function () {
                if (get_attribute(hospital, "type") == 1) {
                    rlnorm3(1,1.5, 0.3 ,service_rate_a)
                } else if(get_attribute(hospital, "type") == 2) {
                    rlnorm3(1,1.5, 0.3 ,service_rate_b)
                } else if(get_attribute(hosptial, "type") == 3) {
                    rlnorm3(1,1.5, 0.3 ,service_rate_c)
                }
            }
        ) %>%
        release_selected() %>%
        #define and update the last patient variable for the OR visited for the wait handler 
        #to be able to select the next patient in an open policy
        #compare the 
        set_global(
            keys = function() {
                x <- get_selected(hospital)
                paste0("Last_Patient_", x)},
            values = function ()  get_attribute(hospital, "ID")
        ) %>%
        set_global(
            keys = function() {
                x <- get_selected(hospital)
                paste0("Number_Of_Last_Patient_", x)},
            values = function ()  as.numeric(gsub(".*?([0-9]+).*", "\\1", get_name(hospital)))
        ) %>%
        #send/pull the next patient to the operating room
        send(signals = function () {
            if(get_selected(hospital) == "OR1") {
                policy <- schedule_policy_OR1
            } else if(get_selected(hospital) == "OR2") {
                policy <- schedule_policy_OR2
            } else if(get_selected(hospital) == "OR2") {
                policy <- schedule_policy_OR3
            }  
            selecting_the_next_patient(policy,get_selected(hospital))}) %>%
        log_( function () paste0("LEAVING ", get_selected(hospital)))
    
    
    
    
    wait_for_patients_handler_OR2 <- trajectory("wait_for_patients_handler") %>%
        
        trap("wait_for_patients_OR2",
             handler = trajectory() %>%
                 #Define variables as attrisbutes to be used 
                 log_(function ()  paste0("Time now: ",as.character(now(hospital)))) %>%
                 # set_attribute("current_entrance", function() get_global(hospital,"Entrance")) %>%
                 #Jump to the time of the next event that is of interest (arrival to the waiting list)
                 set_attribute("Time_to_next_event", function () time_to_next_event()) %>%
                 log_( function () paste0("Time to the next event: ",as.character(get_attribute(hospital,'Time_to_next_event')))) %>%
                 leave(function () {if(get_attribute(hospital,"Time_to_next_event") == -1) 100 else 0 }) %>%
                 timeout_from_attribute("Time_to_next_event") %>%
                 send(function () {selecting_the_next_patient(schedule_policy_OR2, "OR2")}) %>%
                 timeout(0.01) %>%
                 
                 #check if the someone actually came to the OR1 
                 simmer::rollback(7, check = function() {
                     print(paste0("OR 2 server count: ", get_server_count(hospital, "OR2")))  
                     if(get_server_count(hospital, "OR2") == 1) FALSE else TRUE})
             
        ) %>%
        wait()%>%
        rollback(2)
    
    wait_for_patients_handler_OR1 <- trajectory("wait_for_patients_handler") %>%
        
        trap("wait_for_patients_OR1",
             handler = trajectory() %>%
                 #Define variables as attributes to be used 
                 log_(function ()  paste0("Time now: ",as.character(now(hospital)))) %>%
                 # set_attribute("current_entrance", function() get_global(hospital,"Entrance")) %>%
                 #Jump to the time of the next event that is of interest (arrival to the waiting list)
                 set_attribute("Time_to_next_event", function () time_to_next_event()) %>%
                 log_( function () paste0("Time to the next event: ",as.character(get_attribute(hospital,'Time_to_next_event')))) %>%
                 leave(function () {if(get_attribute(hospital,"Time_to_next_event") == -1) 100 else 0 }) %>%
                 timeout_from_attribute("Time_to_next_event") %>%
                 send(function () {selecting_the_next_patient(schedule_policy_OR1, "OR1")}) %>%
                 timeout(0.01) %>%
                 
                 #check if the someone actually came to the OR1 
                 simmer::rollback(7, check = function() {
                     print(paste0("OR 1 server count: ",get_server_count(hospital, "OR1")))  
                     if(get_server_count(hospital, "OR1") == 1) FALSE else TRUE})
             
        ) %>%
        wait()%>%
        rollback(2)
    
    
    t_signaler <- trajectory("signaler") %>%
        send(function() {
            signals <- c()
            d <- get_capacity(hospital, "Day_of_week")
            #IF open schedule policy for both then call the default
            if(schedule_policy_OR1 == "open" & schedule_policy_OR2 == "open" ) {
                type_OR1 <- "a"
                type_OR2 <- "b"
            } else {
                #IF the block scheduling policy is chosen (same as the condition given) then signal the types 
                if(length(which(init_schedule[,1] == "Open")) == 0) {
                    type_OR1 <- get_patient_type_from_schedule_d("OR1",d,init_schedule)
                    type_OR2 <- get_patient_type_from_schedule_d("OR2", d, init_schedule)
                } else {
                    #IF Open is in the schedule it is a modified block schedule 
                    #the open policy room has to take the type that is not scheduled first
                    if(which(init_schedule[,1] == "Open") == 1) {
                        if(get_patient_type_from_schedule_d("OR2", d, init_schedule) == "a") {
                            type_OR1 <- "b"
                            type_OR2 <- get_patient_type_from_schedule_d("OR2", d, init_schedule)
                        }else if(get_patient_type_from_schedule_d("OR2", d, init_schedule) == "b") {
                            type_OR1 <- "a"
                            type_OR2 <- get_patient_type_from_schedule_d("OR2", d, init_schedule)
                        }
                        
                    } else if(which(init_schedule[,1] == "Open") == 2) {
                        if(get_patient_type_from_schedule_d("OR1", d, init_schedule) == "a") {
                            type_OR2 <- "b"
                            type_OR1 <- get_patient_type_from_schedule_d("OR1", d, init_schedule)
                        }else if(get_patient_type_from_schedule_d("OR1", d, init_schedule) == "b") {
                            type_OR2 <- "a"
                            type_OR1 <- get_patient_type_from_schedule_d("OR1", d, init_schedule)
                        }
                    }  
                }
                
            }
            # Signals to send 
            signals <- append(signals,paste0("patient_",type_OR1, 0, " ", "OR1 ", "Free"))
            signals <- append(signals,paste0("patient_",type_OR2, 0, " ", "OR2 ", "Free"))
            print(signals)
            return(signals)   
        }
        )
    
    patients_path_a <- trajectory("patients_path_a") %>%
        set_attribute(keys = "type",values = 1) %>%
        join(patients_path)
    
    patients_path_b <- trajectory("patients_path_b") %>%
        set_attribute(keys = "type",values = 2) %>%
        join(patients_path)
    
    hospital %>%
        add_resource(name = "waiting_list", capacity = Inf)
    
    for (i in seq(number_or)) {
        hospital %>%
            add_resource(name = paste0("OR", i), capacity = 1) %>%
            add_global(paste0("Last_Patient_","OR", i), 0)     
        
    }
    
    hospital %>%
        add_generator("wait_handler_OR1", wait_for_patients_handler_OR1, at(0), mon = 2) %>%
        add_generator("wait_handler_OR2", wait_for_patients_handler_OR2, at(0), mon = 2) %>%
        add_generator("patient_a", patients_path_a,from(0, function () rexp(1,inter_arrival_rate_a)), mon = 2) %>% # at(0,1,3)  
        add_generator("patient_b", patients_path_b,from(0, function () rexp(1,inter_arrival_rate_b)) , mon = 2) %>% #at(0,2,4)
        add_generator("signaler", t_signaler, at(0)) %>%
        add_global("Entrance", 0) %>%
        add_global("Next_Event",0) %>% 
        add_resource("Day_of_week", week) %>%
        
        # Run the simulation 
        run(run_time)
        print(hospital)
    
    
    
}

plot_access_time <- function(simulation) {
    arrival_data <- get_mon_arrivals(simulation)
    
    acc_arrival_data <- arrival_data %>%
        dplyr::filter(!name == "signaler0") %>%
        dplyr::mutate(access_time = end_time - start_time - activity_time) %>%
        dplyr::mutate(type = mapply(function(x) str_sub(x,9,9), name )) %>%
        dplyr::group_by(type) %>%
        dplyr::summarise(mean = mean(access_time),
                         min  = min(access_time),
                         max  = max(access_time),
                         median = median(access_time),
                         n = n())
}

plot_utilization <- function(simulation) {
    resource_data <- get_mon_resources(simulation)
    
    util_resource_data <- resource_data %>%
        dplyr::group_by(resource, replication) %>%
        dplyr::mutate(dt = time - dplyr::lag(time)) %>%
        dplyr::mutate(capacity = ifelse(capacity < server, server, capacity)) %>%
        dplyr::mutate(in_use = dt * dplyr::lag(server / capacity)) %>%
        dplyr::summarise(utilization = sum(in_use, na.rm = TRUE) / sum(dt, na.rm=TRUE)) %>%
        dplyr::filter(resource == "OR1" | resource == "OR2" )
    
}

plot_waiting_list <- function(simulation) {
    resource_data <- get_mon_resources(simulation)
    waiting_list_data <- resource_data %>%
        dplyr::filter(resource == "waiting_list")
    ggplot(waiting_list_data, aes(y = server,x = time)) + geom_line()
}

plot_idle_time <- function(simulation) {
    resource_data <- get_mon_resources(simulation)
    
    idle_time_resource_type_data <- resource_data %>%
        dplyr::mutate(day = (day(seconds_to_period(time * 60 * 60)) %% 5) + 1) %>%
        dplyr::filter(resource == "OR1" | resource == "OR2" ) %>%
        dplyr::mutate(type = mapply( function (resource,day) get_patient_type_from_schedule_d(resource,day, a_init_schedule), resource, day))  %>%
        dplyr::group_by(type) %>%
        dplyr::mutate(dt = time - dplyr::lag(time)) %>%
        dplyr::mutate(capacity = ifelse(capacity < server, server, capacity)) %>%
        dplyr::mutate(in_use = dt * dplyr::lag(server / capacity)) %>%
        dplyr::summarise(idle_time = (sum(dt, na.rm = TRUE) - sum(in_use, na.rm = TRUE)) / sum(dt, na.rm=TRUE)) %>%
        dplyr::filter(type == "a" | type == "b")
    
    ggplot(idle_time_resource_type_data, aes(x= type, y = idle_time)) + geom_col()
}


# Define server logic 
shinyServer(function(input, output, session) {
    
    #-------------------------------------
    # Creation of Appointment Inputs
    #-------------------------------------
    
    
    ## Creating the appointment schedules for Player 1/A 
    a_init_schedule <<- data.frame("Mon" = c("Type A", "Type B", "Type C"),
                                  "Tue" = c("Type A", "Type B", "Type C"),
                                  "Wed" = c("Type A", "Type B", "Type C"),
                                  "Thu" = c("Type A", "Type B", "Type C"),
                                  "Fri" = c("Type A", "Type B", "Type C")
                        )
    row.names(a_init_schedule) <<- c("OR 1", "OR 2", "OR 3")
    
    output$a_input_schedule = renderDT(
        a_init_schedule,
        editable = "cell",
        autoHideNavigation = TRUE,
        options = list(dom = 't'),
        class = "cell-border stripe"
    )
    
    observeEvent(input$a_input_schedule_cell_edit, {
        a_init_schedule <<- editData(a_init_schedule,input$a_input_schedule_cell_edit)
        init_schedules_list <<- list("a_init_schedule" = a_init_schedule,
                                     "b_init_schedule" = b_init_schedule)
        
        print(a_init_schedule)
    })
    
    ## Creating the appointment schedules for Player 2/B 
    b_init_schedule <<- data.frame("Mon" = c("Type A", "Type B", "Type C"),
                                  "Tue" = c("Type A", "Type B", "Type C"),
                                  "Wed" = c("Type A", "Type B", "Type C"),
                                  "Thu" = c("Type A", "Type B", "Type C"),
                                  "Fri" = c("Type A", "Type B", "Type C")
    )
    row.names(b_init_schedule) <<- c("OR 1", "OR 2", "OR 3")
    
    output$b_input_schedule = renderDT(
        b_init_schedule,
        editable = "cell",
        autoHideNavigation = TRUE,
        options = list(dom = 't'),
        class = "cell-border stripe"
    )
    
    observeEvent(input$b_input_schedule_cell_edit, {
        b_init_schedule <<- editData(b_init_schedule,input$b_input_schedule_cell_edit)
        init_schedules_list <<- list("a_init_schedule" = a_init_schedule,
                                    "b_init_schedule" = b_init_schedule)
        print(b_init_schedule)
    })
    
    init_schedules_list <- list("a_init_schedule" = a_init_schedule,
                                "b_init_schedule" = b_init_schedule)
    
 
    
    getparams <- function(prefix) {
        # input[[paste0(prefix, "recalc ")]]
        
        
        varparams <- lapply(varparamNames, function(p) {
            input[[paste0(prefix, "_", p)]]
        })
        names(varparams) <- varparamNames
        
        fixparams <- lapply(fixparamNames, function (p) {
            input[[p]]
        })
        
        names(fixparams) <- fixparamNames
        
        params <- append(varparams, fixparams)
        
        params[["init_schedule"]] <- init_schedules_list[[paste0(prefix,"_init_schedule")]]
        params
    }
    
    
    sim_a <- eventReactive(input$run,{
        print("simualtion A should start")
        do.call(simulate_hospital, getparams("a"))})
    sim_b <- eventReactive(input$run,{
        print("simualtion A should start")
        do.call(simulate_hospital, getparams("b"))})
    
    #cat(file=stderr(), "The simulation enviroment:", "\n", output$sim_a)
    
    ## Assigning the outputs to be rendered in the UI

    #Text/Measure Outputs
    output$a_pAccessTime <- renderTable(plot_access_time(sim_a()))
    output$b_pAccessTime <- renderTable(plot_access_time(sim_b()))
    #
    output$a_utilizationOR <- renderTable(plot_utilization(sim_a()))
    output$b_utilizationOR <- renderTable(plot_utilization(sim_b()))
    #
    # output$a_bedShortage
    # output$b_bedShortage
    # #Plot Outputs
    #
    output$a_lengthOfWaitingList <- renderPlot(plot_waiting_list(sim_a()))
    output$b_lengthOfWaitingList <- renderPlot(plot_waiting_list(sim_b()))
    #
    #
    # output$a_bedOccupancy
    # output$b_bedOccupancy
    #
    #
    output$a_idleTime <- renderPlot(plot_idle_time(sim_a()))
    output$b_idleTime <- renderPlot(plot_idle_time(sim_b()))
    #
})








    