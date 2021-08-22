#dplyr tutorial 

library("nycflights13")
flights
filter(flights, month == 1, day == 1)
jan1 <- filter(flights, month == 1, day == 1)
(jan1 <- filter(flights, month == 1, day == 1))
filter(flights, month == 11 | month == 12)
(nov_dec <- filter(flights, month %in% c(11, 12)))

#Excersises
filter(flights, arr_delay > 120)
filter(flights, dest == "IAH" | dest == "HOU")
filter(flights, carrier == "UA")
filter(flights, arr_time >120 & dep_delay == 0)
filter(flights, between(dep_time, 0, 60*6  ))
filter(flights, is.na(dep_time))


arrange(flights,dep_delay)
arrange(flights, desc(dep_delay))
arrange(flights, desc(dep_delay))
arrange(flights, distance )

dplyr::select(flights, year, month, day)
dplyr::select(flights, -(year:day))
dplyr::select(flights, starts_with("o"))

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
dplyr::select(flights, any_of(vars))
dplyr::select(flights, contains("TIME"))
View(flights)

flights_sml <- dplyr::select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance,
                      air_time)


mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60)

mutate(flights_sml, 
       gain = dep_delay - arr_delay,
       hours = air_time / 60, 
       gain_per_hour = gain / hours
)

(x<- 1:10)
(x-lag(x))

f <- mutate(flights,
       aboard_time = arr_time - dep_time)
dplyr::select(f, aboard_time, air_time, everything())

f<- mutate(flights,
       dep_time2 = sched_dep_time + dep_delay)
dplyr::select(f, dep_time2, dep_time, sched_dep_time, dep_delay)
mutate(flights, 
       arr_delay = min_rank(arr_delay))

### Examples 

not_cancelled <- flights %>% 
      filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
      group_by(year, month, day) %>% 
      summarise(mean = mean(dep_delay))

delays <- not_cancelled %>% 
      group_by(tailnum) %>% 
      summarise(
            delay = mean(arr_delay)
      )

ggplot(data = delays, mapping = aes(x = delay)) + 
      geom_freqpoly(binwidth = 10)

delays <- not_cancelled %>% 
      group_by(tailnum) %>% 
      summarise(
            delay = mean(arr_delay, na.rm = TRUE),
            n = n()
      )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
      geom_point(alpha = 1/10)

delays %>% 
      filter(n > 30) %>% 
      ggplot(mapping = aes(x = n, y = delay)) + 
      geom_point(alpha = 1/10)






