#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     17.10.2019
#modificado: 06.11.2019

setwd("2019/2019.08.13_formacao_RCD2019/bin/")
library("tidyverse")
library("modelr")

##1. Modelling example
library("hexbin")

#1.1.
diamonds2 <- diamonds %>%
  filter((x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10)) %>%
  filter(carat <= 2.5) %>%
  select(carat,cut,color,clarity,price) %>%
  mutate(lprice = log(price), lcarat = log(carat))

diamonds_mod <- lm(lprice ~ lcarat,data=diamonds2)
diamonds2 <- diamonds2 %>%
  add_predictions(diamonds_mod,"lpred") %>%
  add_residuals(diamonds_mod,"lresid") %>%
  mutate(pred = exp(lpred))

diamonds_mod2 <- lm(lprice ~ lcarat + clarity + cut,data=diamonds2)
diamonds2 <- diamonds2 %>%
  add_predictions(diamonds_mod2,"lpred2") %>%
  add_residuals(diamonds_mod2,"lresid2") %>%
  mutate(pred2 = exp(lpred2))

diamonds_mod3 <- lm(lprice ~ lcarat + clarity + cut + color,data=diamonds2)
diamonds2 <- diamonds2 %>%
  add_predictions(diamonds_mod3,"lpred3") %>%
  add_residuals(diamonds_mod3,"lresid3") %>%
  mutate(pred3 = exp(lpred3))

#1.2.
summary(diamonds_mod)
summary(diamonds_mod2)
summary(diamonds_mod3)

#1.3.
diamonds2 %>%
  ggplot() + 
    geom_point(aes(pred,price),size=2,color="grey30") +
    geom_abline(aes(intercept=0,slope=1),size=1,color="white")

diamonds2 %>%
  ggplot() + 
    geom_point(aes(pred2,price),size=2,color="grey30") +
    geom_abline(aes(intercept=0,slope=1),size=1,color="white")

diamonds2 %>%
  ggplot() + 
    geom_point(aes(pred3,price),size=2,color="grey30") +
    geom_abline(aes(intercept=0,slope=1),size=1,color="white")

#1.4.
diamonds2 %>%
  ggplot() + 
    geom_hex(aes(lcarat,lresid),bins=50)

diamonds2 %>%
  ggplot() + 
    geom_hex(aes(lcarat,lresid2),bins=50)

diamonds2 %>%
  ggplot() + 
    geom_hex(aes(lcarat,lresid3),bins=50)

#observations with large residuals
diamonds2 %>%
  filter(abs(lresid3) > 1) %>%
  select(carat:price,pred3,lresid3)

#observations where observed < expected (i.e. under-priced)
diamonds2 %>%
  filter(lresid3 < 0) %>%
  select(carat:price,pred3,lresid3) %>%
  arrange(lresid3)
 
##2. Model Fitting

#2.1.
set.seed(12345)
real_a1 <- 4.22
real_a2 <- 2.05
x <- round(runif(n=30,min=0,max=10),digits=2)
y <- real_a1*x + real_a2 + rnorm(n=30,mean=0,sd=1)
sim1 <- tibble(x,y)

sim1_mod <- loess(y ~ x,data=sim1)

#2.2.
RMSD_loess <- sqrt(mean(sim1_mod$residuals^2))

#2.3.
sim1 <- sim1 %>%
  add_predictions(sim1_mod) %>%
  add_residuals(sim1_mod)

#2.4.
sim1 %>% 
  ggplot() +
    geom_point(aes(x,y),size=2,color="grey30") +
    geom_line(aes(x,pred))

#2.5.
sim1 %>% 
  ggplot() +
    geom_smooth(aes(x,y),method="loess",formula=y ~ x) +
    geom_line(aes(x,pred),color="red")

##3. Model Visualization

#3.1.
set.seed(12345)

real_a1 <- 4.22
real_a2 <- 2.05
x <- round(runif(n=30,min=0,max=10),digits=2)
y <- real_a1*x + real_a2 + rnorm(n=30,mean=0,sd=1)
sim1 <- tibble(x,y)

sim1_mod <- lm(y ~ x,data=sim1)

sim1 <- sim1 %>%
  add_predictions(sim1_mod) %>%
  add_residuals(sim1_mod)

sim1 %>% 
  ggplot() +
    geom_freqpoly(aes(resid,..density..),binwidth=0.5) +
    stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1))

dfnorm <- function(x,mean=0,sd=1,log=FALSE){
    dnorm(x=x,mean=mean,sd=sd,log=log) + dnorm(x=x,mean=-mean,sd=sd,log=log) 
}

sim1 %>% 
  ggplot() +
    geom_histogram(aes(abs(resid),..density..),boundary=0,binwidth=0.5) +
    stat_function(fun=dfnorm,color="red",args=list(mean=0,sd=1))
