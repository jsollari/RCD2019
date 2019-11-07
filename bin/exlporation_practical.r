#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     16.10.2019
#modificado: 06.11.2019

setwd("2019/2019.08.13_formacao_RCD2019/bin/")
library("tidyverse")
library("nycflights13")

#3.1. Missing values

#3.1.1.
flights %>%
  summary()
#flights %>%
#  select_if(function(x){sum(is.na(x))>0}) %>%
#  summarise_all(function(x){sum(is.na(x))})

flights %>%
  summarise(n_NA=sum(is.na(dep_time) | is.na(dep_delay) |
                     is.na(arr_time) | is.na(arr_delay) |
					 is.na(air_time)))
flights %>%
  summarise(n_NA=sum(is.na(air_time)))
#flights %>%
#  transmute(anyNA = dep_time + dep_delay + arr_time + arr_delay + air_time) %>%
#  summarise(n_NA=sum(is.na(anyNA)))
#flights %>%
#  mutate_all(function(x){is.na(x)}) %>%
#  transmute(anyNA = rowSums(.) > 0) %>%
#  summarise(n_NA = sum(anyNA))
#anyNA <- apply(flights,1,function(x){any(is.na(x))})
#anyNA <- !complete.cases(flights)
#n_NA = sum(anyNA)

#flights %>%
#  drop_na()      #works for any subset of columns
#na.omit(flights) #only works for all columns

#3.1.2. 
not_cancelled <- flights %>%
  filter(!is.na(air_time))

#3.1.3.
not_cancelled %>%
  group_by(year,month,day) %>%
  summarize(mean = mean(dep_delay))

#3.2. Outliers

#3.2.1.
not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay))

#3.2.2.
not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay)) %>%
  ggplot() +
   geom_freqpoly(mapping=aes(x=delay),binwidth=10)

#3.2.3.  
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay),
            n_flights = n())

#3.2.4.
delays %>% 
  ggplot() +
    geom_point(mapping=aes(x=n_flights,y=delay),alpha=1/10)
  
#3.2.5.
delays %>%
  filter(n_flights > 25) %>%
  ggplot() +
   geom_point(mapping=aes(x=n_flights,y=delay),alpha=1/10)
   
#3.3. Distribuição 1D

#3.3.1.
diamonds2 <- diamonds %>%
  filter((x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10))

diamonds2_scale <- diamonds2 %>%
  mutate(price = (price - mean((price)))/sd(price))

diamonds2_scale %>%
  ggplot() +
    geom_histogram(mapping=aes(x=price,y=..density..),binwidth=0.2) +
    stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1))

#3.3.2
param <- diamonds2_scale %>%
  summarize(n = n(),
            mean = mean(price),
			sd = sd(price),
            skewness = skewness(price),
			kurtosis = kurtosis(price))
print(round(cbind(t(param),c(1/0,0,1,0,3)),digits=3))

#3.3.3.
diamonds2_scale <- diamonds2 %>%
  mutate(carat = (carat - mean(carat))/sd(carat))

diamonds2_scale %>%
  ggplot() +
    geom_histogram(mapping=aes(x=carat,y=..density..),binwidth=0.2) +
    stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1))

param <- diamonds2_scale %>%
  summarize(n = n(),
            mean = mean(carat),
			sd = sd(carat),
            skewness = skewness(carat),
			kurtosis = kurtosis(carat))
print(round(cbind(t(param),c(1/0,0,1,0,3)),digits=3))

#3.3.4.
ldiamonds2_scale <- diamonds2 %>%
  mutate(carat = (log(carat) - mean(log(carat)))/sd(log(carat)))

ldiamonds2_scale %>%
  ggplot() +
    geom_histogram(mapping=aes(x=carat,y=..density..),binwidth=0.2) +
    stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1))

param <- ldiamonds2_scale %>%
  summarize(n = n(),
            mean = mean(carat),
			sd = sd(carat),
            skewness = skewness(carat),
			kurtosis = kurtosis(carat))
print(round(cbind(t(param),c(1/0,0,1,0,3)),digits=3))
  
#3.4. Distribuição 2D

#3.4.1.
diamonds2 <- diamonds %>%
  filter((x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10))

diamonds2 %>%
  filter(cut=="Fair") %>%
  ggplot() +
    geom_boxplot(mapping=aes(x=clarity,y=price))

diamonds2 %>%
  filter(cut=="Fair") %>%
  group_by(clarity) %>%
  summarize(mean_price = mean(price))

diamonds2 %>%
  filter(cut=="Fair") %>%
  group_by(clarity) %>%
  mutate(mean_price = mean(price)) %>%
  ggplot() +
    geom_point(mapping=aes(x=mean_price,y=price),size=2,alpha=0.01) + 
    geom_point(mapping=aes(x=mean_price,y=mean_price),size=2,color="red")

#3.4.2.
diamonds2 %>%
  filter(cut=="Fair") %>%
  group_by(clarity) %>%
  mutate(mean_price = mean(price)) %>%
  ungroup() %>%
  summarize(cor(mean_price,price))

#3.4.3.

#Calculate Cramer's V from contingency table  
calc_Cramers_V <- function(x){
    chi_stat <- chisq.test(x)$statistic #chi-sqrt
    n <- sum(x)                         #sample size
    min_dim <- min(dim(x)) - 1          #minimum number of dimensions - 1 
    res <- sqrt(chi_stat/n/min_dim)     #Cramer's V
    names(res) = "Cramers.V"
    return(res)
}

diamonds2 %>%
  group_by(clarity,color) %>%
  summarize(n = n()) %>%
  ggplot() +
    geom_tile(mapping=aes(x=clarity,y=color,fill=n))
cont_table <- table(diamonds2$clarity,diamonds2$color) #contingency table
print(cont_table)
round(calc_Cramers_V(cont_table),digits=3)

diamonds2 %>%
  group_by(cut,color) %>%
  summarize(n = n()) %>%
  ggplot() +
    geom_tile(mapping=aes(x=cut,y=color,fill=n))
cont_table <- table(diamonds2$cut,diamonds2$color) #contingency table
print(cont_table)
round(calc_Cramers_V(cont_table),digits=3)
 