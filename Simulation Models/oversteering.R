#######OVERSTEERING PART 2 #####################################################
#Using a scheudle object to schedule capacity changes
#capacity changes from time 0 to 25 to 1, from 25 to 50 to 0 and so on
# it reeccurs in the period of 100 time units
capacity_schedule <- schedule(timetable =c(25, 50, 75, 100),
                              values = c(1,1,1,1),period = 100)
# Initialize the simulation at the beginning
oversteer <- simmer("oversteer")
# Define trajectories to follow
normal_a<- trajectory("normal_a") %>%
  # Set capacity dynamically based on reactive functions
  #set_capacity("oc_a", function() {oc_a_capacity()} ) %>%
  # Follow the surgical chain
  seize("oc_a") %>%
  timeout(10) %>%
  release("oc_a") %>%
  # Set capacity dynamically based on reactive functions
  #note: capactiy only changes if some patient is going through
  #this trajectory
  #set_capacity("or_a", function() {or_a_capacity()} ) %>%
  seize("or_a") %>%
  timeout(10) %>%
  release("or_a")
normal_b <- trajectory("normal_b") %>%
  # Set capacity dynamically based on reactive functions
  #set_capacity("oc_b", function() {oc_b_capacity()} ) %>%
  # Follow the surgical chain
  seize("oc_b") %>%
  timeout(10) %>%
  release("oc_b") %>%
  #set_capacity("or_b", function() {or_b_capacity()} ) %>%
  seize("or_b") %>%
  timeout(10) %>%
  release("or_b")
normal_c <- trajectory("normal_c") %>%
  # Set capacity dynamically based on reactive functions
  #set_capacity("oc_c", function() {oc_c_capacity()} ) %>%
  # Follow the surgical chain
  seize("oc_c") %>%
  timeout(10) %>%
  release("oc_c") %>%
  #set_capacity("or_c", function() {or_c_capacity()} ) %>%
  seize("or_c") %>%
  timeout(10) %>%
  release("or_c")
# Build the environment with resources and generators
oversteer %>%
  add_resource("oc_a", capacity_schedule) %>%
  add_resource("oc_b", capacity = 1) %>%
  add_resource("oc_c", capacity = 1) %>%
  add_resource("or_a", capacity_schedule) %>%
  add_resource("or_b", capacity = 1) %>%
  add_resource("or_c", capacity = 1) %>%
  add_generator("patient_a", normal_a, function() {10}, mon = 2) %>%
  add_generator("patient_b", normal_b, function() {10}, mon = 2) %>%
  add_generator("patient_c", normal_c, function() {10}, mon = 2)

oversteer %>% run(until = 100)

#Trying things with ggplot 

pl <- subset(get_mon_resources(oversteer), resource == c("oc_a", "oc_b", "oc_c"))
             
ggplot(data = NULL, mapping = aes(time, queue)) + 
  geom_line(data = pl, aes(color = factor(resource))) +
  geom_line(data = oc_capacity_stat, aes(x = time, y = queue))      #+ facet_grid(var(resource))
  



get_mon_arrivals(oversteer, per_resource = TRUE) %>%
   transform(waiting_time = end_time - start_time - activity_time)



### Get the queue size for the whole Outpatient clinc so all patient types combined

# get monitored resources
resource_stat <- get_mon_resources(oversteer)

# build empty data frame to store the new values for the whole oc 
oc_capacity_stat <- data.frame(time = vector(), queue = vector() )

# loop over all times and sum up the queue sizes at these times from all patient types
# add the queue size per time to the data frame 
for(i in resource_stat[, "time"]) {
  queue_size <- sum(resource_stat[resource_stat$time == i,  "queue"])
  oc_capacity_stat <- rbind(oc_capacity_stat, data.frame(time = i, queue = queue_size))
}

# get the unique values since some times are computet multiple times
oc_capacity_stat <- unique(oc_capacity_stat)
# plot the data for all oc's
ggplot(data = oc_capacity_stat, mapping = aes(time, queue)) + geom_line()
# plot the data for all oc's and the cumulative for the big one oc 
ggplot(data = NULL, mapping = aes(time, queue)) + 
     geom_line(data = pl, aes(color = factor(resource))) +
     geom_line(data = oc_capacity_stat, aes(x = time, y = queue, size = 1))      #+ facet_grid(var(resource))

