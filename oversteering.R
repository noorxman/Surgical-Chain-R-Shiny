#test outpatient vs operating room oversteering
library(simmer)
library(simmer.plot)

oversteer <- simmer("oversteer")
cap <- 0 #dynamic changes in path can occur with the use of branch and a variable
# Callable functions can change their value dynamically
# monitoring of resources works as well: plot(get_mon_resource(oversteer))
normal <- trajectory("normal") %>%
      log_("I am in normal path") %>%
      seize("oc") %>%
      timeout(10) %>% 
      release("oc") %>% 
      log_("oh boy") %>%
      seize("or") %>% 
      timeout(10) %>%
      release("or")

patient <- trajectory("patient") %>%
      branch( option = function() {cap}, continue = TRUE,
              # option 1: CHange the capacity of oc to 0 
              trajectory() %>%
                    log_("The capactiy was changed to 0") %>%
                    set_capacity("oc", 0 ),
              # option 2: change the capacity of oc back to 1
              trajectory() %>%
                    log_("the capacity has been increased to 1") %>%
                    set_capacity("oc", 1)
              ) %>%
      log_("I am joining the normal path") %>%
      join(normal)

oversteer %>%
      add_resource('oc', capacity = 1) %>%
      add_resource("or", capacity = 1) %>%
      add_generator("patient", patient, function() {10})

#oversteer %>% run(until = 100)
