################################################################################
######################## FINAL SIMULATION MODEL ################################
################################################################################

set.seed(2021)

library(simmer)
library(ggplot2)
library(simmer.plot)
library(zoo) #rollmean function
library(EnvStats) #rlnorm3 function

                        ### SOME VARIABLES ###

arrival_rate <- function() { rexp(1,rate = 1/15) } # mean = 1/rate = 10
CHECK_REVISIT <- function() {runif(1) <= 0.2} # revisit rate 20%

      ## Capacity variables
cap_doc <- 5 #number of doctors available for oc and or 
#Checks if the schedule is valid for the given doctor capacity and the time intervalls
create_capacity_schedules <- function(oc_capacity, or_capacity,time_intervall = c(25,50,75,100))
   {
   period <- time_intervall[length(time_intervall)]
   # x <- list(...)
   # oc_capacity <- x[1:length(time_intervall)]
   # or_capacity <- x[(length(time_intervall)+1):length(x)]
   
   if(length(oc_capacity) == length(or_capacity)) {
      # Check if the doctor capacity was exceeded
      for (i in 1:length(oc_capacity)) {
         if(oc_capacity[[i]] + or_capacity[[i]] > cap_doc) {
            stop(paste("The number of doctors was exceeded in phase", i))
         }
      }
      oc_schedule <- schedule(timetable = time_intervall,
                              values = as.numeric(oc_capacity), period = period)
      or_schedule <- schedule(timetable = time_intervall,
                              values = as.numeric(or_capacity), period = period)
      list(oc_schedule,or_schedule)
   } else {
     stop("The capacity vectors for oc and or are not the same length
          or the time intervall length is not equal to the capacity vector length")
   }
   
   
}

#Only call with a schedule data frame that has the days as columns and OC or OR as values
df_to_schedule <- function(data) {
   y <- c()
   for(i in 1:length(names(data))) {
      #Create the OR schedule
      if(data[1,i] == "OR") {
         append(y,1) 
      } else {
         append(y,0)
         
      }
   }
   x
}

# create_schedule <- function(Q1,Q2,Q3,Q4) {
#    schedule(timetable =c(25, 50, 75, 100),
#             values =  c(Q1,Q2,Q3,Q4), period = 100)
# } 
#possible to create a schedule with that 
# and to implement some rules like if(sum(Q1,Q2,Q3,Q4) < 4) then yes otherwise no
sched <- create_capacity_schedules(oc_capacity = c(1,2,1,2), or_capacity = c(1,1,1,1),
                                   time_intervall = c(100,200,300,400))
cap_oc <- sched[[1]]
cap_or <- sched[[2]]
cap_wd <- 10 # may be infinite since people need a bet necessarily

      ## Service Times
serv_oc <- function () {rlnorm3(1,2.5,0.5)} #which values to use for th three parameters
serv_or <- function () {rlnorm3(1,2.5,0.5)} #they were tested with hist() and trial and error
serv_wd <- function () {rlnorm3(1,2.5,0.5)}

            ### INSTANTIATE THE SIMULATION ENVIROMENT ###

hospital <- simmer("hospital")

                  ### DEFINE TRAJECTORIES ###

patient_normal <- trajectory("patients normal path") %>%
      
      ## add consultation activity with doctor at oc
      log_("Begin OC") %>%
      seize("outpatient_clinic") %>%
      timeout(serv_oc) %>%
      release("outpatient_clinic") %>%
      log_("End OC, Waiting at 'home' for OR ") %>%
      
      ## Patient is waiting at home for the appointment to take place 
      # Here it is modeled as waiting in the OR queue as he get the next appointement 
      # that is free according to the scheudle of the doctor which is given by 
      # changing the capacity of the objects with a schedyle object. 
      
      ## Goes from 'home' to operating room and surgery is performed
      seize("operating_room") %>%
      log_("Beginning Surgery") %>%
      timeout(serv_or) %>%
      release("operating_room") %>%
      log_("Surgery done, going to ward") %>% 
      
      ## After surgery the patient goes to the ward for some amount and then leaves
      #set_capacity("ward", 1, mod = "+") %>% 
      seize("ward") %>%
      timeout(serv_wd) %>%
      release("ward") %>% 
      log_("Treatement done")  #%>%
      #set_capacity("ward", -1, mod = "+")

      ## Some patient have to revisit so they are going back to the OC again

      # branch(CHECK_REVISIT, continue = FALSE, trajectory() %>%
      #                                           log_("Revisit going to the OC again") %>%
      #                                           seize("outpatient_clinic") %>%
      #                                           timeout(serv_oc) %>%
      #                                           release("outpatient_clinic") %>%
      #                                           log_("Revisit Treatement done"))
      # 

            ### FILL THE HOSPITAL ENVIROMENT WITH RESOURCES AND GENERATORS ###
      
hospital  %>%
      add_resource("outpatient_clinic",cap_oc) %>%
      add_resource("operating_room",cap_or) %>%
      add_resource("ward", cap_wd) %>%
      add_generator("patient", patient_normal, arrival_rate, mon = 2) 
      
            ### RUN THE SIMULATION ###
hospital %>% run(until = 100)

            ### COMPUTE METRICS ###

# Classics 
plot(get_mon_resources(hospital), metric = "usage", items = "queue",
     c("outpatient_clinic", "operating_room", "ward"))

plot(get_mon_resources(hospital), metric = "utilization",
     c("outpatient_clinic", "operating_room", "ward"))

# Turn scientific notation on or off (999) is off 
options(scipen = 0)

      ## Acess Time (between OC and OR)

# Get the arrivals per resource and compute waiting times
arrival <- get_mon_arrivals(hospital, per_resource = TRUE) %>%
   transform(waiting_time = round (end_time - start_time - activity_time, digits = 3)) #round up to 3 digits

# Filter out the operating room waiting times = access times and the important columns
access_times_or <- subset(arrival, resource == "operating_room")[,c("name", "waiting_time")]
names(access_times_or) <- c("name", "access_time")

# Plot the access time data ??? Still not sure what the best plot is
ggplot(data = access_times_or, aes( x = "access_time")) + geom_bar()


      ## Access Time between end of ward and revisit OR 

calculate_access_time_revisit <- function() {
   
   table(arrival$name) #frequency of each name
   tf <- as.data.frame(table(arrival$name))# make it into a dataframe
   tn <- tf[tf$Freq > 3, "Var1"] # get the names of the patients that appear >3 times
   tn <- as.vector(tn) #make it into an vector
   arrival$name %in% tn # check if in arrival$name there are elements of tn
   
   # Only the arrivals that appear more than 4 times are stored
   access_times_revisit <- subset(arrival,arrival$name %in% tn )
   x <- data.frame( name = vector(), waiting_time = vector())
   
   for(i in access_times_revisit$name) {
      x <- rbind(x,subset(access_times_revisit, name == i)[4, c("name", "waiting_time")])
   }
   unique(x)
}

# Calculating the mean 
mean(calculate_access_time_revisit()$waiting_time)
