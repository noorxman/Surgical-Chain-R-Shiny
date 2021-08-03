#OR Scheduling Simulation Model 
library(dplyr)
library(simmer)
library(stringr)

#Test input schedule

a_init_schedule <- data.frame("Mon" = c("Type A", "Type B", "Type C"),
                              "Tue" = c("Type B", "Type B", "Type C"),
                              "Wed" = c("Type A", "Type B", "Type C"),
                              "Thu" = c("Type A", "Type B", "Type C"),
                              "Fri" = c("Type B", "Type B", "Type C"))

#Initialize simulation enviroment
hospital <- simmer("hospital")

#Define Trajectories
patients_path <- trajectory("patients_path") %>%
   set_global("Entrance", values = 1,mod = "+") %>%
   set_attribute("ID", function() {get_global(hospital, "Entrance")}) %>%
   seize("waiting_list") %>%
   #log_(function() {paste0("Message to be received: ",get_name(hospital), " ", "OR1 Free")}) %>%
   trap(signals = function() {paste0(get_name(hospital), " ", "OR1 Free")},
        handler = trajectory() %>%
           log_("Message received") %>%
           release("waiting_list")
   ) %>%
   log_("WAITING") %>%
   wait() %>% 
   simmer::select("OR1") %>% #function(){sample(c("OR1", "OR2"),1)}
   log_("GOING OR1") %>%
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
   send(signals = function () {selecting_the_next_patient("block")}) %>%
   log_("LEAVING OR1")


# wait_for_patients_handler <- trajectory("wait_for_patients_handler") %>%
#    
#    trap("wait_for_patients",
#         handler = trajectory() %>%
#            #Define variables as attributes to be used 
#            log_(function ()  paste0("Time now: ",as.character(now(hospital)))) %>%
#            set_attribute("current_entrance", function() get_global(hospital,"Entrance")) %>%
#            #Jump to the time of the next event that is of interest (arrival to the waiting list)
#            set_attribute("Time_to_next_event", function() {
#               x <- peek(hospital, steps = 5, verbose = TRUE)
#               x <- x[!x$process == "wait_handler0" & x$time != Inf,]
#               if(!is.na(x[1,1])) {
#                  return(x[1,1]- now(hospital) + 1)
#               } else {
#                  return(-1)
#                      
#               }
#               
#               }) %>%
#            log_( function () paste0("Time to the next event: ",as.character(get_attribute(hospital,'Time_to_next_event')))) %>%
#            leave(function () {if(get_attribute(hospital,"Time_to_next_event") == -1) 100 else 0 }) %>%
#            timeout_from_attribute("Time_to_next_event") %>%
#            log_(function () paste0("New Entry is :",get_global(hospital,"Entrance") - 1 )) %>%
#            send(function () paste0("patient_a",get_global(hospital,"Entrance") - 1, " ", "OR1 Free")) %>%
#            log_(function () paste0("Message send is: ","patient_a",
#                                    get_global(hospital,"Entrance") - 1, " ", "OR1 Free" ))%>%
#            timeout(1) %>%
#             
# 
#            rollback(8, check = function() {
#               if(get_global(hospital,"Entrance") > get_attribute(hospital,"current_entrance"))
#                  FALSE else TRUE})
# 
#    ) %>%
#    wait()%>%
#    rollback(2)

wait_for_patients_handler_OR1 <- trajectory("wait_for_patients_handler") %>%
   
   trap("wait_for_patients",
        handler = trajectory() %>%
           #Define variables as attributes to be used 
           log_(function ()  paste0("Time now: ",as.character(now(hospital)))) %>%
           # set_attribute("current_entrance", function() get_global(hospital,"Entrance")) %>%
           #Jump to the time of the next event that is of interest (arrival to the waiting list)
           set_attribute("Time_to_next_event", function() {
              x <- peek(hospital, steps = 5, verbose = TRUE)
              x <- x[!x$process == "wait_handler0" & x$time != Inf,]
              if(!is.na(x[1,1])) {
                 return(x[1,1]- now(hospital) + 0.001)
              } else {
                 return(-1)
                 
              }
              
           }) %>%
           log_( function () paste0("Time to the next event: ",as.character(get_attribute(hospital,'Time_to_next_event')))) %>%
           leave(function () {if(get_attribute(hospital,"Time_to_next_event") == -1) 100 else 0 }) %>%
           timeout_from_attribute("Time_to_next_event") %>%
           send(function () {select_next_waiting_patient("block"
                                                         # function (){ # get the type of the patient that called the waiting handler
                                                         #    x <- get_mon_arrivals(hospital, per_resource = TRUE,ongoing = TRUE)
                                                         #    x <- x[x$resource == "OR1",]
                                                         #    p_name <- x[1,1]
                                                         #    if(str_detect(p_name, "b")) "b" else "a"
                                                         # }
                                                         )}) %>%
           timeout(1) %>%
           
           #check if the someone actually came to the OR1 
           rollback(8, check = function() {
              print(paste0(get_server_count(hospital, "OR1")))  
              if(get_server_count(hospital, "OR1") == 1) FALSE else TRUE})
        
   ) %>%
   wait()%>%
   rollback(2)

select_next_waiting_patient <- function (policy) {
   if(policy == "open") {
      select_next_waiting_patient_open()
   } else if (policy == "block") {
      select_next_waiting_patient_block()
   }
}

select_next_waiting_patient_open <- function () {
   #PRECONDITION: NO one is in the waiting list = wait handler is called correctly
   #filter the arrivals that happen at the predicted next moment
   x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
   x <- x[x$resource == "waiting_list",]
   x <- x[order(x$start_time),]
   x <- x[!duplicated(x$name),]
   x <- x[x$start_time == now(hospital) - 0.001,]
   # take the patient that arrives at this moment as he is the newest addition to the waiting list
   for (i in 1:length(x$name)) {
      #patient_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", x[i,1])) ONLY FOR BLOCK Policy
      print(paste0("The next one is: ",x[i,1], " ", "OR1 ", "Free" ))
      return(paste0(x[i,1], " ", "OR1 ", "Free") )
   }
   
}

select_next_waiting_patient_block <- function () {
   #PRECONDITION: NO one of the specified type is in the waiting list = wait handler is called correctly
   #filter the arrivals that happen at the predicted next moment
   patient_type <- get_patient_type_from_schedule()
   
   #check if no one of the type is in the schedule 
   
   
   x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
   x <- x[x$resource == "waiting_list",]
   x <- x[order(x$start_time),]
   x <- x[!duplicated(x$name),]
   x <- x[str_detect(str_sub(x$name,9,9), patient_type),]#filter the patient type
   
   #intermediate check to see if someone suitable is in the queue (especially for swithcing types in block)
   patient_in_waiting <- selecting_the_next_patient_block_for_waiting()
   
   if(length (x[x$name == patient_in_waiting,1]) >0) {
      return(paste0(patient_in_waiting, " ", "OR1 ", "Free"))
      }
   
   x <- x[x$start_time == now(hospital) - 0.001,]
   
   # take the next patient of the specified type
   if(!is.na(x[1,1])) {
         for (i in 1:length(x$name)) {
      
         print(paste0("The next one is: ",x[i,1], " ", "OR1 ", "Free" ))   
         return(paste0(x[i,1], " ", "OR1 ", "Free") )
         }
   }
   #if no one is found trigger the waiting handler again to repeat the steps and jump to the next event
   return("wait_for_patients")
}

t_signaler <- trajectory("signaler") %>%
   send(function() {
      signals <- c()
      type <- "a"
      n_to_schedule <- 0 #mean service time / total time of the day
      for (i in 0:n_to_schedule) {
         signals <- append(signals,paste0("patient_",type, i, " ", "OR1 ", "Free"))
      }
      print(signals)
      return(signals)   
   }
   )


send_mulitple_signals <- function() {
   signals <- c()
   type <- "a"
   n_to_schedule <- 3 #mean service time / total time of the day
   for (i in 1:n_to_schedule) {
      signals <- append(signals,paste0("patient_",type, i, " ", "OR1 ", "Free"))
   }
   print(signals)
   return(signals)
}

get_patient_type_from_schedule <- function () {
   x <- now(hospital)
   max_run_time <- 1000
   day_time_range <- 24
   
   for (i in 0:as.integer(max_run_time/day_time_range)) {
      if( i %% 2 == 0) {
         #Monday
         if(between(x,0 + day_time_range * i, 24 + 24 * i)) {
            switch (a_init_schedule[1,1], #a_init_schedule[1,1] means monday column and row of OR1
                    "Type A" = return("a"),
                    "Type B" = return("b") # instead of 2 make it "b"
            )
         }
         
      } else {
         if(between(x,0 + day_time_range * i, 24 + 24 * i)) {
            switch (a_init_schedule[1,2], #a_init_schedule[1,2] means tuesday column and row of OR1
                    "Type A" = return("a"),
                    "Type B" = return("b") 
            )
         }
         
      }
      
   }
   
}

selecting_the_next_patient <- function(policy) {
   if(policy == "open") {
      selecting_the_next_patient_open()
   } else if (policy == "block") {
      selecting_the_next_patient_block()
   }
   #if(OR is defined as an open schedule ) then do selcting the next patient open function 
   
   #if(OR is defined as block scheduling) then do selectin the next patient block function 
   
   #the modified block scheduling case is done by having on operating room open while the others are block
}

selecting_the_next_patient_open <- function() {
   #filter the arrival table to the arrivals in the wating list in ascending order
   x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
   x <- x[x$resource == "waiting_list",]
   x <- x[order(x$start_time),]
   x <- x[!duplicated(x$name),]
   # get the patient that is after this patient in the waiting list,
   # independent of the patient type (open scheduling)
   for (i in 1:length(x$name)) {
      if(get_name(hospital) == x[i,1]) {
         if(!is.na(x[i +1, 1])) {
            return(paste0(x[i+1,1], " ", "OR1 ", "Free") )
         }
      }
   }
   # if there is no one after this patient initiate the wait for patient handler 
   return("wait_for_patients")
   
}

selecting_the_next_patient_block_for_waiting <- function () {
   #Extract the number of the current patients
   # patient_name <- get_name(hospital)
   # patient_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", patient_name))
   patient_type <- get_patient_type_from_schedule()
   
   #filter the finsihed and unfinished arrivals of a particlular patient type in ascending order
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
}

selecting_the_next_patient_block <- function() {
   #Extract the number of the current patients
   # patient_name <- get_name(hospital)
   # patient_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", patient_name))
   patient_type <- get_patient_type_from_schedule()
   
   #filter the finsihed and unfinished arrivals of a particlular patient type in ascending order
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
         return(paste0(next_patient_name, " ", "OR1 ", "Free"))
      } else {
         return("wait_for_patients")
      }   
   } else {return("wait_for_patients")}
   
   
   }
  
   
   


patients_path_a <- trajectory("patients_path_a") %>%
   set_attribute(keys = "type",values = 1) %>%
   join(patients_path)

patients_path_b <- trajectory("patients_path_b") %>%
   set_attribute(keys = "type",values = 2) %>%
   join(patients_path)

#Define Resources
hospital %>%
   add_resource(name = "waiting_list", capacity = Inf) #limit queue size to allow for a max waiting list length

#Add the variable number of operating rooms and their global attributes
for (i in 1:3) {
   hospital %>% add_resource(name = paste0("OR", i), capacity = 1) %>%
      add_global(paste0("Last_Patient_","OR", i), 0)             
   
}

# #Add the correct start time of the first signaler that initializes the pull process of the OR's
# start_time_signaler <- function() {
#    x <- peek(hospital, steps = 4, verbose = TRUE)
#    x <- x[x$process == "patient_a",]
#    return(at(x[1,1]))
# }

#Add generators
hospital %>%
   add_generator("wait_handler", wait_for_patients_handler_OR1, at(0), mon = 2) %>%
   add_generator("patient_a", patients_path_a, from(0, function () rexp(1,0.1)), mon = 2) %>%
   add_generator("patient_b", patients_path_b, from(0, function () rexp(1,0.1)), mon = 2) %>% 
   add_generator("signaler", t_signaler, at(0)) %>%
   add_global("Entrance", 0) %>%
   add_global("Next_Event",0)


#Run the Simulation





#Test Functions###############################################################
select_next_test <- function(name) {  
   for (i in 1:length(x$name)) {
      print(i)
      if(name == x[i,1]) {
         print(paste0(i," found"))
         if(!is.na(x[i +1, 1])) {
            print(paste0("next one ",x[i+1,1]))
            return(x[i+1, 1])
         }
      }
      
   }
   print("There is no next on in the waiting list")
}

get_patient_type_from_schedule_test <- function (time) {
   x <- time
   max_run_time <- 1000
   day_time_range <- 24
   
   for (i in 0:as.integer(max_run_time/day_time_range)) {
      if( i %% 2 == 0) {
         #Monday
         if(between(x,0 + day_time_range * i, 24 + 24 * i)) {
            switch (a_init_schedule[1,1], #a_init_schedule[1,1] means monday column and row of OR1
                    "Type A" = return("a"),
                    "Type B" = return("b") # instead of 2 make it "b"
            )
         }
         
      } else {
         if(between(x,0 + day_time_range * i, 24 + 24 * i)) {
            switch (a_init_schedule[1,2], #a_init_schedule[1,2] means tuesday column and row of OR1
                    "Type A" = return("a"),
                    "Type B" = return("b") 
            )
         }
         
      }
      
   }
   
}
