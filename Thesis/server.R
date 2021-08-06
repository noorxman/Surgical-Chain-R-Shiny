#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(stringr)
library(purrr)
library(lubridate)
library(dplyr)
library(simmer)
library(simmer.plot)
library(shiny)

# Define server logic 
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
    
    ## Setting up some fixed variables
    
        #define the week cycle -> 5 days 
    week <- schedule(c(24,48,72,96,120), c(2,3,4,5,1), period = 120)
    
    number_or <- 2
    
    run_time <- 200
    
    
    ## Defining functions independent from simulation environment 
    
    get_patient_type_from_schedule_d <- function (or, d) {
        
        room <- as.numeric(str_sub(or,3))
        
        if(a_init_schedule[room, d] == "Type A") {
            return("a")
        }else if(a_init_schedule[room, d] == "Type B") {
            return("b")
        }else if(a_init_schedule[room, d] == "Type C") {
            return("c")
        }else {
            return("open")
        }
        
    }
    
    
    
    
    sim_a <- eventReactive(input$run,{
        
        define_policy <- function(policy) {
            if(policy == 1) {
                schedule_policy_OR1 <<- "open"
                schedule_policy_OR2 <<- "open"
            } else if (policy == 2) {
                schedule_policy_OR1 <<- "block"
                schedule_policy_OR2 <<- "block"
            } else if (policy == 3 & length(which(a_init_schedule[,1] == "Open"))) {
                #make the schedule with the modified block asput open in the table instead of Type A (maybe also dependend on the weekday
                if(which(a_init_schedule[,1] == "Open") == 1) {
                    schedule_policy_OR1 <<- "open"
                    schedule_policy_OR2 <<- "block"
                } else if(which(a_init_schedule[,1] == "Open") == 2) {
                    schedule_policy_OR1 <<- "block"
                    schedule_policy_OR2 <<- "open"
                }
                
            }
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
        
        select_next_waiting_patient <- function (policy,or) {
            if(policy == "open") {
                select_next_waiting_patient_open(or)
            } else if (policy == "block") {
                select_next_waiting_patient_block(or)
            }
        }
        
        select_next_waiting_patient_open <- function (or) {
            selecting_the_next_patient_open(or)
            print(selecting_the_next_patient_open(or))
            # #PRECONDITION: NO one is in the waiting list = wait handler is called correctly
            # #filter the arrivals that happen at the predicted next moment
            # x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
            # z <- get_mon_arrivals(hospital)
            # x <- x[x$resource == "waiting_list",]
            # x <- x[order(x$start_time),]
            # x <- x[!duplicated(x$name),]
            # 
            # #Preliminary check if someone came to the waiting list while the wait handler jumped in time
            # # p_name <- selecting_the_next_patient_open_for_waiting(or)
            # # 
            # # if(!p_name == paste0("wait_for_patients_",or)) {
            # #     print(paste0("The next one is: ",p_name))
            # #     return(p_name)
            # # }
            # 
            # # Continue with checking the new event 
            # 
            # x <- x[x$start_time >= now(hospital) - 0.0015,]
            # # take the patient that arrives at this moment as he is the newest addition to the waiting list
            # if(!is.na(x[1,1])) {
            #     print(paste0("The next one is: ",x[1,1], " ", or," ", "Free" ))
            #     return(paste0(x[1,1], " ", or, " ", "Free") )
            # } else { return(paste0("wait_for_patients_",or))}
            
        }
        
        select_next_waiting_patient_block <- function (or) {
            #PRECONDITION: NO one of the specified type is in the waiting list = wait handler is called correctly
            #filter the arrivals that happen at the predicted next moment
            patient_type <- get_patient_type_from_schedule(or)
            
            #check if no one of the type is in the schedule 
            
            
            x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
            x <- x[x$resource == "waiting_list",]
            x <- x[order(x$start_time),]
            x <- x[!duplicated(x$name),]
            x <- x[str_detect(str_sub(x$name,9,9), patient_type),]#filter the patient type
            
            #intermediate check to see if someone suitable is in the queue (especially for swithcing types in block)
            patient_in_waiting <- selecting_the_next_patient_block_for_waiting(or)
            
            if(length (x[x$name == patient_in_waiting,1]) >0) {
                return(paste0(patient_in_waiting, " ", or," ", "Free"))
            }
            
            x <- x[x$start_time == now(hospital) - 0.0015,]
            
            # take the next patient of the specified type
            if(!is.na(x[1,1])) {
                for (i in 1:length(x$name)) {
                    
                    print(paste0("The next one is: ",x[i,1], " ", or, " ", "Free" ))   
                    return(paste0(x[i,1], " ", or," ", "Free") )
                }
            }
            #if no one is found trigger the waiting handler again to repeat the steps and jump to the next event
            return(paste0("wait_for_patients_", or))
        }
        
        get_patient_type_from_schedule <- function (or) {
            
            day <- get_capacity(hospital, "Day_of_week")
            room <- as.numeric(str_sub(or,3))
            
            if(a_init_schedule[[room, day]] == "Type A") {
                return("a")
            }else if(a_init_schedule[room, day] == "Type B") {
                return("b")
            }else if(a_init_schedule[room, day] == "Type C") {
                return("c")
            }
            
        }
        
        
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
            x <- x[!(duplicated(x$name) | duplicated(x$name, fromLast = TRUE)),]
            x <- x[order(x$start_time),]
            print("This is the table for the next open patient:")
            print(x)
            
            if(!is.na(x[1,1])) {
                result <- paste0(x[1,1], " ", or, " ", "Free")
                print(paste0("This is the resulting next patient", result))
                return(result)
            } else {
                print("No suitable patient is in the queue")
                return(paste0("wait_for_patients_",or))
            }
            # patients that are in the other operating rooms at the moment
            
            #people that are still not finished
            # x <- x[x$resource == "waiting_list",]
            # x <- x[order(x$start_time),]
            # x <- x[!duplicated(x$name),]
            # x <- x[is.na(x$end_time),] 
            # 
            
            # get the patient that is after this patient in the waiting list,
            # independent of the patient type (open scheduling)
            
            # f <- which(x$name == get_name(hospital))
            # # special case for the beginning due to the fact that the first ones are registered as done in the table and do not have the NA  
            # if(get_name(hospital) == "patient_a0") {
            #    f <- 0
            # }
            # 
            # if(!is.na(x[1,1]) & !length(f) == 0) {
            #    for (i in f:length(x$name)) {
            #       if(!is.na(x[i +1, 1]) & !x[i +1, 1] %in% y$name ) {
            #          result <- paste0(x[i+1,1], " ", or, " ", "Free")
            #          return(result)
            #       }
            #       
            #    }
            # }
            # if there is no one after this patient initiate the wait for patient handler 
            
            
        }
        
        
        # selecting_the_next_patient_open_for_waiting <- function(or) {
        #     #filter the arrival table to the arrivals in the wating list in ascending order
        #     x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
        #     z <- get_mon_arrivals(hospital)
        #     # patients that are in the other operating rooms at the moment
        #     y <- x[!x$resource == or & !x$resource == "waiting_list",] 
        #     #people that are still not finished
        #     x <- x[x$resource == "waiting_list",]
        #     x <- x[order(x$start_time),]
        #     x <- x[!duplicated(x$name),]
        #     
        #     
        #     # get the patient that is after this patient in the waiting list,
        #     # independent of the patient type (open scheduling)
        #     
        #     f <- which(x$name == get_name(hospital))
        #     # special case for the beginning due to the fact that the first ones are registered as done in the table and do not have the NA  
        #     if(get_name(hospital) == "patient_a0" | get_name(hospital) == "patient_b0") {
        #         f <- 0
        #     }
        #     
        #     if(!is.na(x[1,1]) & !length(f) == 0) {
        #         for (i in f:length(x$name)) {
        #             if(!is.na(x[i +1, 1]) & !x[i +1, 1] %in% y$name ) {
        #                 result <- paste0(x[i+1,1], " ", or, " ", "Free")
        #                 return(result)
        #             }
        #             
        #         }
        #     }
        #     # if there is no one after this patient initiate the wait for patient handler 
        #     return(paste0("wait_for_patients_",or))
        #     
        # }
        
        selecting_the_next_patient_block_for_waiting <- function (or) {
            #Extract the number of the current patients
            # patient_name <- get_name(hospital)
            # patient_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", patient_name))
            patient_type <- get_patient_type_from_schedule(or)
            
            #filter the finished and unfinished arrivals of a particlular patient type in ascending order
            x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
            x <- x[x$resource == "waiting_list",]
            x <- x[order(x$start_time),]
            y <- x[!is.na(x$end_time),] #y is the people that are finished
            #filter based on patient type 
            y <- y[str_detect(str_sub(y$name,9,9), patient_type),] # patient that are done
            x <- x[str_detect(str_sub(x$name,9,9), patient_type),] # all patient that were in queue
            # removing duplicates
            x <- x[!duplicated(x$name),]
            #consider the patient that was last done of this specific type 
            next_patient_num <-nrow(y) #gives the number of the patient that is next in line for that type
            
            next_patient_name <- paste0("patient_",patient_type, next_patient_num )
        }
        
        selecting_the_next_patient_block <- function(or) {
            #Extract the number of the current patients
            # patient_name <- get_name(hospital)
            # patient_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", patient_name))
            patient_type <- get_patient_type_from_schedule(or)
            
            #filter the finished and unfinished arrivals of a particular patient type in ascending order
            x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
            x <- x[x$resource == "waiting_list",]
            x <- x[order(x$start_time),]
            y <- x[!is.na(x$end_time),] #y is the people that are done
            #filter based on patient type 
            y <- y[str_detect(str_sub(y$name,9,9), patient_type),] # patient that are done
            x <- x[str_detect(str_sub(x$name,9,9), patient_type),] # all patient that were in queue
            # removing duplicates
            x <- x[!duplicated(x$name),]
            #consider the patient that was last done of this specific type 
            next_patient_num <-nrow(y) #gives the number of the patient that is next in line for that type
            
            next_patient_name <- paste0("patient_",patient_type, next_patient_num )
            #check if the next patient is actually in the waiting list otherwise call the wait handler
            chosen_one <- x[x$name == next_patient_name, 1]
            if(length(chosen_one) != 0) {
                if (chosen_one == next_patient_name ) {
                    return(paste0(next_patient_name, " ", or, " ", "Free"))
                } else {
                    return(paste0("wait_for_patients_",or))
                }   
            } else {return(paste0("wait_for_patients_",or))}
            
            
        }
        
        ####END OF FUNCTIONS####
        
        define_policy(input$a_schedulePolicies)
        
        #Initialize simulation environment
        hospital <- simmer("hospital")
        print("teST")
        
        #Define Trajectories
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
            timeout(5) %>%
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
                     send(function () {select_next_waiting_patient(schedule_policy_OR2, "OR2")}) %>%
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
                     send(function () {select_next_waiting_patient(schedule_policy_OR1, "OR1")}) %>%
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
                #IF open schedule policy for both then call the default
                if(schedule_policy_OR1 == "open" & schedule_policy_OR2 == "open" ) {
                    type_OR1 <- "a"
                    type_OR2 <- "b"
                } else {
                    #IF the block scheduling policy is chosen (same as the condition given) then signal the types 
                    if(length(which(a_init_schedule[,1] == "Open")) == 0) {
                        type_OR1 <- get_patient_type_from_schedule("OR1")
                        type_OR2 <- get_patient_type_from_schedule("OR2")
                    } else {
                        #IF Open is in the schedule it is a modified block schedule 
                        #the open policy room has to take the type that is not scheduled first
                        if(which(a_init_schedule[,1] == "Open") == 1) {
                            if(get_patient_type_from_schedule("OR2") == "a") {
                                type_OR1 <- "b"
                                type_OR2 <- get_patient_type_from_schedule("OR2")
                            }else if(get_patient_type_from_schedule("OR2") == "b") {
                                type_OR1 <- "a"
                                type_OR2 <- get_patient_type_from_schedule("OR2")
                            }
                            
                        } else if(which(a_init_schedule[,1] == "Open") == 2) {
                            if(get_patient_type_from_schedule("OR1") == "a") {
                                type_OR2 <- "b"
                                type_OR1 <- get_patient_type_from_schedule("OR1")
                            }else if(get_patient_type_from_schedule("OR1") == "b") {
                                type_OR2 <- "a"
                                type_OR1 <- get_patient_type_from_schedule("OR1")
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
            add_generator("patient_a", patients_path_a,from(0, function () rexp(1,0.2)), mon = 2) %>% # at(0,1,3)  
            add_generator("patient_b", patients_path_b, from(0, function () rexp(1,0.2)) , mon = 2) %>% #at(0,2,4)
            add_generator("signaler", t_signaler, at(0)) %>%
            add_global("Entrance", 0) %>%
            add_global("Next_Event",0) %>% 
            add_resource("Day_of_week", week) %>%
            
            #Run the simulation 
            run(run_time)
        print(hospital)
        
        
        
        
    })
    
    #cat(file=stderr(), "The simulation enviroment:", "\n", output$sim_a)
    
    ## Assigning the outputs to be rendered in the UI

    #Text/Measure Outputs
    output$a_pAccessTime <- renderTable(
        {
            #Access time summary
            arrival_data <- get_mon_arrivals(sim_a())
            
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
    )
                                          
    # output$b_pAccessTime
    # 
    output$a_utilizationOR <- renderTable(
        {   resource_data <- get_mon_resources(sim_a())
            
        util_resource_data <- resource_data %>%
                dplyr::group_by(resource, replication) %>%
                dplyr::mutate(dt = time - dplyr::lag(time)) %>%
                dplyr::mutate(capacity = ifelse(capacity < server, server, capacity)) %>%
                dplyr::mutate(in_use = dt * dplyr::lag(server / capacity)) %>%
                dplyr::summarise(utilization = sum(in_use, na.rm = TRUE) / sum(dt, na.rm=TRUE)) %>%
                dplyr::filter(resource == "OR1" | resource == "OR2" )
        
        
        }
    )
    # output$b_utilizationOR
    # 
    # output$a_bedShortage
    # output$b_bedShortage
    # #Plot Outputs 
    # 
    output$a_lengthOfWaitingList <- renderPlot(
        {resource_data <- get_mon_resources(sim_a())
        
            waiting_list_data <- resource_data[resource_data$resource == "waiting_list",]
            
            ggplot(data = waiting_list_data, aes(y = server,x = time)) + geom_line() 
        }
    )
    # output$b_lengthOfWaitingList
    # 
    # 
    # output$a_bedOccupancy
    # output$b_bedOccupancy
    # 
    # 
    output$a_idleTime <- renderPlot(
        {resource_data <- get_mon_resources(sim_a())
        
        idle_time_resource_type_data <- resource_data %>%
            dplyr::mutate(day = (day(seconds_to_period(time * 60 * 60)) %% 5) + 1) %>%
            dplyr::filter(resource == "OR1" | resource == "OR2" ) %>%
            dplyr::mutate(type = mapply( function (resource,day) get_patient_type_from_schedule_d(resource,day), resource, day))  %>%
            dplyr::group_by(type) %>%
            dplyr::mutate(dt = time - dplyr::lag(time)) %>%
            dplyr::mutate(capacity = ifelse(capacity < server, server, capacity)) %>%
            dplyr::mutate(in_use = dt * dplyr::lag(server / capacity)) %>%
            dplyr::summarise(idle_time = (sum(dt, na.rm = TRUE) - sum(in_use, na.rm = TRUE)) / sum(dt, na.rm=TRUE)) %>%
            dplyr::filter(type == "a" | type == "b")
        
        ggplot(idle_time_resource_type_data, aes(x= type, y = idle_time)) + geom_col()
        }
    )
    # output$b_idleTime
    # 
})
