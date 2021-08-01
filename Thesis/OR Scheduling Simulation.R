#OR Scheduling Simulation Model 
library(simmer)

#Initialize simulation enviroment
hospital <- simmer("hospital")

#Define Trajectories
patients_path <- trajectory("patients_path") %>%
   set_global("Entrance", values = 1,mod = "+") %>%
   set_attribute("ID", function() {get_global(hospital, "Entrance")}) %>%
   seize("waiting_list") %>%
   log_(function() {paste0("to be received: ",get_name(hospital), " ", "OR1 Free")}) %>%
   trap(signals = function() {paste0(get_name(hospital), " ", "OR1 Free")},
        handler = trajectory() %>%
           log_("Leaving the waiting list for the OR1") %>%
           release("waiting_list")
   ) %>%
   log_("In the Waiting List") %>%
   wait() %>% 
   select("OR1") %>% #function(){sample(c("OR1", "OR2"),1)}
   log_("Going to the OR1") %>%
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
      values = function ()  as.numeric(gsub(".*?([0-9]+).*", "\\1", x[i,1]))
   ) %>%
   #send/pull the next patient to the operating room
   send(signals = function () {selecting_the_next_patient_open()}
      # #filter the arrivals in the waiting list and in ascending time order
      # x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
      # x <- x[x$resource == "waiting_list",]
      # x <- x[order(x$start_time),]
      # x <- x[!duplicated(x$name),]
      # # get the patient that arrived after the one that is leaving the OR 
      #    for (i in 1:length(x$name)) {
      #       if(get_name(hospital) == x[i,1]) {
      #          if(!is.na(x[i +1, 1])) {
      #             print(paste0("The next one is: ",x[i+1,1]))
      #             return(paste0(x[i+1,1], " ", "OR1 ", "Free") )
      #          }
      #       }
      #    }
      #     
      # # if there is no one that arrived after the one that is leaving the OR at the time he leaves, 
      # # initiate the wait for patients handler that checks the next patient that arrives to the waiting list
      # return("wait_for_patients")
      # 
      # 
    ) %>%
   
   log_("Leaving the OR1")


wait_for_patients_handler <- trajectory("wait_for_patients_handler") %>%
   
   trap("wait_for_patients",
        handler = trajectory() %>%
           #Define variables as attributes to be used 
           log_(function ()  paste0("Time now: ",as.character(now(hospital)))) %>%
           set_attribute("current_entrance", function() get_global(hospital,"Entrance")) %>%
           #Jump to the time of the next event that is of interest (arrival to the waiting list)
           set_attribute("Time_to_next_event", function() {
              x <- peek(hospital, steps = 5, verbose = TRUE)
              x <- x[!x$process == "wait_handler0" & x$time != Inf,]
              if(!is.na(x[1,1])) {
                 return(x[1,1]- now(hospital) + 1)
              } else {
                 return(-1)
                     
              }
              
              }) %>%
           log_( function () paste0("Time to the next event: ",as.character(get_attribute(hospital,'Time_to_next_event')))) %>%
           leave(function () {if(get_attribute(hospital,"Time_to_next_event") == -1) 100 else 0 }) %>%
           timeout_from_attribute("Time_to_next_event") %>%
           log_(function () paste0("New Entry is :",get_global(hospital,"Entrance") - 1 )) %>%
           send(function () paste0("patient_a",get_global(hospital,"Entrance") - 1, " ", "OR1 Free")) %>%
           log_(function () paste0("Message send is: ","patient_a",
                                   get_global(hospital,"Entrance") - 1, " ", "OR1 Free" ))%>%
           timeout(1) %>%
            

           rollback(8, check = function() {
              if(get_global(hospital,"Entrance") > get_attribute(hospital,"current_entrance"))
                 FALSE else TRUE})

   ) %>%
   wait()%>%
   rollback(2)

wait_for_patients_handler_OR1 <- trajectory("wait_for_patients_handler") %>%
   
   trap("wait_for_patients",
        handler = trajectory() %>%
           #Define variables as attributes to be used 
           log_(function ()  paste0("Time now: ",as.character(now(hospital)))) %>%
           set_attribute("current_entrance", function() get_global(hospital,"Entrance")) %>%
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
           send(function () {select_next_waiting_patient_open()}) %>%
           timeout(1) %>%
           
           #check if the someone actually came to the OR1 
           rollback(8, check = function() {
              print(paste0(get_server_count(hospital, "OR1")))  
              if(get_server_count(hospital, "OR1") == 1) FALSE else TRUE})
        
   ) %>%
   wait()%>%
   rollback(2)

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

selecting_the_next_patient <- function(OR) {
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
   # if there is no one after this patient initate the wait for patient handler 
   return("wait_for_patients")
   
}

selecting_the_next_patient_block <- function() {
   #filter the arrival table to the arrivals in the wating list in ascending order
   x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
   x <- x[x$resource == "waiting_list",]
   x <- x[order(x$start_time),]
   x <- x[!duplicated(x$name),]
   # get the patient that is after this patient in the waiting list that is the same type
   # as the patient calling him (block scheduling)
   
   #Extract the number of the current patinet
   patient_name <- get_name(hospital)
   patient_num <- as.numeric(gsub(".*?([0-9]+).*", "\\1", patient_name))
   patient_type <- get_attribute(hospital, "type")
   if(patient_type == 1) {
      patient_type = "a"
   } else if (patient_type == 2) {
      patient_type = "b"
   }
   next_patient_name <- paste0("patient_", patient_type, patient_num + 1)
   #get the next patient of the same type 
   for (i in 1:length(x$name)) {
      if(next_patient_name == x[i,1]) {
         return(paste0(x[i,1], " ", "OR1 ", "Free") )
         }
   }
   
   return("wait_for_patients")
   }
   # if there is no one after this patient initate the wait for patient handler 
   
   


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
