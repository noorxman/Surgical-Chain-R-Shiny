## block, signal and continue with a handler
signal <- "you shall pass"
signal2 <- "you shall pass2"

t_blocked <- trajectory() %>%
      trap(
            signal,
            trajectory() %>%
                  log_("executing the handler1"), interruptible = FALSE) %>%
      
      trap(
            signal2,
            trajectory() %>%
                  log_("executing the handler2"), interruptible = FALSE) %>%
      log_("waiting...") %>%
      wait() %>%
      log_("continuing!")

t_signaler <- trajectory() %>%
      log_(signal2) %>%
      send(signal2) %>%
      log_(signal) %>%
      send(signal) 

simmer() %>%
      add_generator("blocked", t_blocked, at(0)) %>%
      add_generator("signaler", t_signaler, at(5)) %>%
      run() %>% invisible
#> 0: blocked0: waiting...
#> 5: signaler0: you shall pass
#> 5: blocked0: executing the handler
#> 5: blocked0: continuing!
## handlers can be interrupted, unless interruptible=FALSE
# t_worker <- trajectory() %>%
#       trap(
#             signal,
#             handler = trajectory() %>%
#                   log_("ok, I'm packing...") %>%
#                   timeout(1)) %>%
#       log_("performing a looong task...") %>%
#       timeout(100) %>%
#       log_("and I'm leaving!")
# 
# simmer() %>%
#       add_generator("worker", t_worker, at(0)) %>%
#       add_generator("signaler", t_signaler, at(5, 5.5)) %>%
#       run() %>% invisible#> 0: worker0: performing a looong task...
#> 5: signaler0: you shall pass
#> 5: worker0: ok, I'm packing...
#> 5.5: signaler1: you shall pass
#> 5.5: worker0: ok, I'm packing...
#> 6.5: worker0: and I'm leaving!




