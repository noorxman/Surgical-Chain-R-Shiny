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
      timeout(10) %>%
      release_selected() %>%
      send(signals = function() {
         x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
         x <- x[x$resource == "waiting_list",]
         x <- x[order(x$start_time),]
         x <- x[!duplicated(x$name),]
         
         for (i in 1:length(x$name)) {
            if(get_name(hospital) == x[i,1]) {
               if(!is.na(x[i +1, 1])) {
                  print(paste0("The next one is: ",x[i+1,1]))
                  return(paste0(x[i+1,1], " ", "OR1 ", "Free") )
               }
            }
         }
         return("wait_for_patients")
         
      } ) %>%
      log_("Leaving the OR1")

wait_for_patients_handler <- trajectory("wait_for_patients_handler") %>%
      trap("wait_for_patients",
           handler = trajectory()
              
                        ) %>%
      wait()%>%
      rollback(2)

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

handle_signal <- function() {
      paste0(get_name(hospital), "OR1 Free")
}
      
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

selecting_the_next_one <- function() {
      x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
      x <- x[x$resource == "waiting_list",]
      x <- x[order(x$start_time),]
      x <- x[!duplicated(x$name),]
      
      for (i in 1:length(x$name)) {
            if(get_name(hospital) == x[i,1]) {
                  if(!is.na(x[i +1, 1])) {
                        return(x[i+1,1])
                  }
            }
      }
      return("wait_for_patients")
      
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
#Add generators
hospital %>%
      add_generator("patient_a", patients_path_a, at(0,1,2,6),mon = 2) %>%
      add_generator("signaler", t_signaler, at(0)) %>%
      add_global("Entry", 0)

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
