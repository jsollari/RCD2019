#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     14.10.2019
#modificado: 05.11.2019

setwd("2019/2019.08.13_formacao_RCD2019/bin/")
library("tidyverse")

## 1. filter()
library("nycflights13")
?flights

#table
print(flights)
dim(flights)

#filter 1
jan1 <- flights %>%
  filter(month==1,day==1)
print(jan1)
dim(jan1)

#filter 2
#nov_dec <- flights %>%
#  filter(month==11 | month==12)
nov_dec <- flights %>%
  filter(month %in% c(11,12))
print(nov_dec)
dim(nov_dec)

#filter 3
#no_late <- flights %>% 
#  filter(!(arr_delay > 120 | dep_delay > 120))
no_late <- flights %>%
  filter(arr_delay <= 120 & dep_delay <= 120)
print(no_late)
dim(no_late)

## 2. arrange()
library("nycflights13")
?flights

#table
print(flights)

#arrange 1
ord_date <- flights %>% 
  arrange(year,month,day)
print(ord_date)
all.equal(flights,ord_date)

#arrange 2
ord_arr_delay <- flights %>%
  arrange(desc(arr_delay))
print(ord_arr_delay)

## 3. select()
library("nycflights13")
?flights

#table
print(flights)

#select 1
flights %>%
  select(year,month,day)

#select 2
flights %>%
  select(year:day)

#select 3
flights %>%
  select(-(year:day))

#select 4
flights %>%
  select(contains("arr"))
#flights[,grep("arr",colnames(flights))]

#select 4
flights %>%
  select(time_hour,air_time,everything())

#rename 1
flights %>%
  rename(tail_num=tailnum)
#flights_temp <- flights
#colnames(flights_temp) <- sub("tailnum","tail_num",colnames(flights_temp))
#flights_temp

## 4. mutate()
library("nycflights13")
?flights

#table
flights_sml <- flights %>% 
  select(year:day,ends_with("delay"),distance,air_time)
print(flights_sml)

#mutate 1
flights_sml %>%
  mutate(gain = arr_delay - dep_delay,
                speed = distance/air_time*60)
                   
#mutate 2
flights_sml %>%
  mutate(gain = arr_delay - dep_delay,
         hours = air_time/60,
         gain_per_hour = gain/hours)

#transmute 1
flights_sml %>%
  transmute(gain=arr_delay - dep_delay,
            hours = air_time/60,
            gain_per_hour = gain/hours)

## 5. summarize()
library("nycflights13")
?flights

#table
print(flights)

#summarize 1
flights %>% 
  summarize(delay = mean(dep_delay,na.rm=TRUE)) 

#summarize 2
flights %>%
  group_by(dest) %>%
  summarize(count = n(),
            dist = mean(distance,na.rm=TRUE),
            delay = mean(dep_delay,na.rm=TRUE)) %>%
  filter(count > 20,dest != "HNL")

#summarize 3
delay <- group_by(.data=flights,dest)
delay <- summarize(.data=by_dest,count = n(),
                                 dist = mean(distance,na.rm=TRUE),
                                 delay = mean(dep_delay,na.rm=TRUE))
delay <- filter(.data=delay,count > 20,dest != "HNL")
print(delay)

## 6. group_by()
library("nycflights13")
?flights

#table
print(flights)

#group 1
flights %>% 
  group_by(year) %>%
  summarize(nflights = n())

#group 2
daily <- flights %>% 
  group_by(year,month,day)
print(daily)
per_day <- daily %>%
  summarize(nflights = n())
print(per_day)
per_month <- per_day %>%
  summarize(nflights = sum(nflights))
print(per_month)
per_year <- per_month %>%
  summarize(nflights = sum(nflights))
print(per_year)

#ungroup 1
daily %>%
  ungroup() %>%
  summarize(nflights = n())
