#OR Scheduling Simulation Model 
library(simmer)

#Initialize simulation enviroment
hospital <- simmer("hospital")

#Define Trajectories
patients_path <- trajectory("patients_path") %>%
      seize("waiting_list") %>%
      log_(function() {paste0(get_name(hospital), " ", "OR1 Free")}) %>%
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
      log_("Leaving the OR1")

t_signaler <- trajectory("signaler") %>%
      send(function() {
            signals <- c()
            type <- "a"
            n_to_schedule <- 3 #mean service time / total time of the day
            for (i in 1:n_to_schedule) {
                  signals <- append(signals,paste0("patient_",type, i, " ", "OR1 ", "Free"))
            }
            print(signals)
            return(signals)
            }
      )

handle_signal <- function() {
      paste0(get_name(hospital), "OR1 Free")
}
      
send_signals <- function() {
      signals <- c()
      type <- "a"
      n_to_schedule <- 3 #mean service time / total time of the day
      for (i in 1:n_to_schedule) {
            append(signals,paste0("patient_",type, i))
            
      }
      return(signals)
}

selecting_the_next_one <- function() {
      x <- get_mon_arrivals(hospital, ongoing = TRUE, per_resource = TRUE)
      x <- x[x$resource == "waiting_list",]
      x <- x[order(x$start_time),]
      x <- x[!duplicated(x$name),]
      
      for (i in 1:length(x$name)) {
            if(get_name() == x[i,1]) {
                  if(!is.na(x[i +1, 1])) {
                        return(x[i+1,1])
                  }
            }
      }
      
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
      
#Add the variable number of operating rooms
for (i in 1:3) {
      hospital %>% add_resource(name = paste0("OR", i), capacity = 1)
      
}
#Add generators
hospital %>%
      add_generator("patient_a", patients_path_a, at(0,1,2,6),mon = 2) %>%
      add_generator("signaler", t_signaler, at(5))

#Run the Simulation




#Test Functions###############################################################
select_next_test <- function(name) {  
      for (i in 1:length(x$name)) {
            print(i)
            if(name == x[i,1]) {
                  print(paste0(i," found"))
                  if(!is.na(x[i +1, 1])) {
                        
                        print(paste0("next one ",x[i+1,1]))
                  }
            }
            
      }
}