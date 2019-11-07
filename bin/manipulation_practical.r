#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     21.10.2019
#modificado: 06.11.2019

setwd("2019/2019.08.13_formacao_RCD2019/bin/")
library("tidyverse")
library("nycflights13")

#2.1. filter()
 
#2.1.1.
flights %>%
  filter(arr_delay > 120)

#2.1.2.
flights %>%
  filter(dest %in% c("IAH","HOU"))

#2.1.3.
flights %>%
  filter(arr_delay > 120 & dep_delay <=0)

#2.1.4.
flights %>%
  filter(between(dep_time, 0, 600))

#2.2. arrange()

#2.2.1.
flights %>%
  arrange(desc(dep_delay))  %>%
  select(dep_delay)

#2.2.2.
flights %>%
  mutate(speed = distance/air_time*60) %>%
  arrange(speed)  %>%
  select(speed)
  
#2.2.3.
flights %>%
  arrange(desc(distance)) %>%
  select(distance)
  
flights %>%
  arrange(desc(distance)) %>%
  select(distance)

#2.2.4.
flights %>%
  arrange(desc(is.na(air_time))) %>%
  select(air_time)
  
##2.3. select()

#2.3.1.
flights %>%
  select(year:day,day)

#2.3.2.
vars1 <- c("year","years")
flights %>%
  select(one_of(vars1))
vars2 <- c("year","month","day","time")
flights %>%
  select(one_of(vars2))

##2.4. mutate()

#2.4.1.
flights %>%
  transmute(dep_time = dep_time,
            dep_hours = dep_time%/%100,
            dep_minutes = dep_time%%100)
flights %>%
  transmute(dep_time = dep_time,
            dep_hours = ifelse(nchar(dep_time)==3,
			                   as.numeric(substr(dep_time,1,1)),
			                   as.numeric(substr(dep_time,1,2))),
            dep_minutes = ifelse(nchar(dep_time)==3,
			                     as.numeric(substr(dep_time,2,3)),
			                     as.numeric(substr(dep_time,3,4))))

#2.4.2.
flights %>%
  transmute(dep_delay = dep_delay,
            dep_delay2 = (dep_time%/%100 - sched_dep_time%/%100)*60 +
			              dep_time%%100 - sched_dep_time%%100)

##2.5. summarize()

#2.5.1.
flights %>%
  group_by(year,month,day) %>%
  summarize(first_flight = min(dep_time,na.rm=TRUE),
            last_flight = max(dep_time,na.rm=TRUE))

#2.5.2.
flights %>%
  group_by(dest) %>%
  summarize(n_carriers = n_distinct(carrier))

#2.5.3.
flights %>%
  group_by(dep_time < 500) %>%
  summarize(n_flights = n())

#2.5.4.
flights %>%
  group_by(year,month,day) %>%
  summarize(prop_flights = mean(arr_delay > 60,na.rm=TRUE))
