#####Simulating a simple health center with revisits  (Surgical chain edition)
set.seed(2021)
library(simmer)
library(simmer.plot)

      ### SOME CONSTANTS ###
arrival_rate <- function() { 10 }#rexp(1,1/5)}
##Capacity
cap_oc <- 1
cap_or <- 1
cap_wd <- 1
##Service Times
serv_oc <- 10  #for stochastic values use function() rnorm(1,15)
serv_or <- 30
serv_wd <- 10

CHECK_REVISIT <- function() {runif(1) <= 0.5} #50% of patients revisit

############## SIMULATION ENVIROMENT ###########################################

      ### INSTANTIATE THE SIMULATION ENVIROMENT ###
#Instatiate at the begiining to let hospital be in scope for the trajectories at runtime 
hospital <- simmer("hospital")

      ### DEFINE TRAJECTORIES ###

patient_normal <- trajectory("patients normal path") %>%
      ## add nurse intake activity
      seize("outpatient_clinic", amount = 1) %>%
      timeout(serv_oc) %>%
      release("outpatient_clinic") %>%
      
      ## add consultation activity with doc
      set_attribute("oc_end", function() {now(hospital)}) %>% # setting attribute to compute access time
      log_( function () {
            paste("Waiting for or from:", get_attribute(hospital, "oc_end"))
            
      }) %>%
      seize("operating_room",amount = 1) %>%
      set_attribute("access_time_or", function() {
            now(hospital) - get_attribute(hospital, "oc_end") # access time is the time between release from oc and entering or 
      }) %>%
      log_( function () {
            paste("Waiting for Operating room ended at :", now(hospital) ) 
            
      }) %>%
      timeout(serv_or) %>%
      release("operating_room", amount = 1) %>%
      
      ##add a planning activity
      seize("ward") %>%
      timeout(serv_wd) %>%
      release("ward") 

patient_revisit <- trajectory("patients revisit path") %>%
      
      set_attribute("ward_end", function() {now(hospital)}) %>% # setting attribute to compute access time
      log_( function () {
            paste("Waiting for new app. at home from:", get_attribute(hospital, "ward_end")) 
            }) %>%
      
      seize("home") %>% 
      timeout(10) %>% 
      release("home") %>% 
      set_attribute("access_time_home", function() {
            now(hospital) - get_attribute(hospital, "ward_end") # access time is the time between release from oc and entering or 
      }) %>%
            log_( function () {
                  paste("Going to the outpatient clinic at :", now(hospital) ) 
                  }) %>%
      
      join(patient_normal)
      
      

patient <- trajectory("patients path with revisit") %>%
      join(patient_normal) %>%
      branch(CHECK_REVISIT, continue = FALSE, patient_revisit)

      ### FILL THE HOSPITAL ENVIROMENT WITH RESOURCES AND GENERATORS ###

hospital  %>%
      add_resource("outpatient_clinic",cap_oc) %>%
      add_resource("operating_room",cap_or) %>%
      add_resource("ward", cap_wd) %>%
      add_resource("home", capacity = Inf) %>% 
      add_generator("patient", patient, arrival_rate, mon = 2) 

      ### RUN THE SIMULATION ###
#Always seperate the initialisation of the enviroment and the run method because
#of scoping rules

#for replications use:j
#hospital <- lapply(1:100,function(){ INSERT SIMULATION ENVIOREMENT})

hospital %>% run(until = 100)
      ### ANALYSING/CALCULATING METRICS ### 

### ANALYSE AND PLOT RESULTS use ?plot.mon
plot(get_mon_resources(hospital), metric = "usage")

plot(get_mon_resources(hospital), metric = "usage", items = "queue", steps = TRUE)

plot(get_mon_resources(hospital), metric = "utilization")


plot(get_mon_arrivals(hospital), metric = "activity_time")

plot(get_mon_arrivals(hospital), metric = "waiting_time")

plot(get_mon_arrivals(hospital), metric = "flow_time")

### HOW TO COMPUTE 

##Get the attributes data frame
hospital %>% get_mon_attributes()
#This frame will have the intermediate calculation of the time as well so: 

##Subset the frame such that only the entries where the access times != time are stored
subset(get_mon_attributes(hospital), key == "access_time_or" )

#subset(get_mon_attributes(hospital), time != value)


##Get access time for home or for operating room

#subset(get_mon_attributes(hospital), (time != value) & key == "access_time_or")
