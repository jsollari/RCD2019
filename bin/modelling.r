#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     17.10.2019
#modificado: 05.11.2019

setwd("2019/2019.08.13_formacao_RCD2019/bin/")
library("tidyverse")
library("modelr")

## 1. Modelling example
library("hexbin")

#diamonds data
?diamonds

fnam <- "../results/modelling/diamonds1.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
   geom_boxplot(aes(cut,price))
dev.off()

fnam <- "../results/modelling/diamonds2.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
   geom_boxplot(aes(clarity,price))
dev.off()

fnam <- "../results/modelling/diamonds3.tiff"
tiff(file=fnam,units="in",width=4,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
   geom_hex(aes(carat,price),bins=50)
dev.off()

diamonds2 <- diamonds %>%
  filter((x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10)) %>%
  filter(carat <= 2.5) %>%
  select(carat,cut,color,clarity,price) %>%
  mutate(lprice = log(price), lcarat = log(carat))

fnam <- "../results/modelling/diamonds4.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_hex(aes(lcarat,lprice),bins=50)
dev.off()

#model 1
diamonds_mod <- lm(lprice ~ lcarat,data=diamonds2)
summary(diamonds_mod)

diamonds2 <- diamonds2 %>%
  add_predictions(diamonds_mod,"lpred") %>%
  add_residuals(diamonds_mod,"lresid") %>%
  mutate(pred = exp(lpred))

fnam <- "../results/modelling/diamonds5.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_hex(aes(carat,price),bins=50) +
    geom_line(aes(carat,pred),color="red")
dev.off()

fnam <- "../results/modelling/diamonds6.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_boxplot(aes(cut,lresid))
dev.off()

fnam <- "../results/modelling/diamonds7.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_boxplot(aes(clarity,lresid))
dev.off()

fnam <- "../results/modelling/diamonds8.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_hex(aes(lcarat,lresid),bins=50)
dev.off()

fnam <- "../results/modelling/diamonds9.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_point(aes(pred,price)) +
    geom_abline(aes(intercept=0,slope=1),size=1,color="white")
dev.off()

#model 2
diamonds_mod2 <- lm(lprice ~ lcarat + clarity + cut,data=diamonds2)
summary(diamonds_mod2)

diamonds2 <- diamonds2 %>%
  add_predictions(diamonds_mod2,"lpred2") %>%
  add_residuals(diamonds_mod2,"lresid2") %>%
  mutate(pred2 = exp(lpred2))

fnam <- "../results/modelling/diamonds10.tiff"
tiff(file=fnam,units="in",width=9,height=4.5,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_hex(aes(carat,price),bins=50) +
    geom_line(aes(carat,pred2),color="red") +
    facet_grid(cut ~ clarity)
dev.off()

fnam <- "../results/modelling/diamonds11.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_hex(aes(lcarat,lresid2),bins=50)
dev.off()

fnam <- "../results/modelling/diamonds12.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_point(aes(pred2,price)) +
    geom_abline(aes(intercept=0,slope=1),size=1,color="white")
dev.off()
 
## 2. Model Fitting
set.seed(12345)

#synthetic data
real_a1 <- 4.22
real_a2 <- 2.05
x <- round(runif(n=30,min=0,max=10),digits=2)
y <- real_a1*x + real_a2 + rnorm(n=30,mean=0,sd=1)
sim1 <- tibble(x,y)

#plot synthetic data
fnam <- "../results/modelling/synthetic1.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
sim1 %>%
  ggplot() +
    geom_point(aes(x,y))
dev.off()

#### 2.1. fit model (random search)
models <- tibble(
  a1 = runif(250,-20,40),
  a2 = runif(250,-5,5))

fnam <- "../results/modelling/synthetic2.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
sim1 %>%
  ggplot() +
    geom_abline(data=models,aes(intercept=a1,slope=a2),alpha=0.24) + 
    geom_point(aes(x,y))
dev.off()

#Linear regression model
model1 <- function(a,dat){
  res <- a[1] + dat$x*a[2]
  return(res)
}

#Calculate Root-mean-squared-deviation
measure_distance <- function(params,dat){
  diff <- dat$y - model1(params,dat)
  res <- sqrt(mean(diff^2))
  return(res)
}

#Helper function to calculate RMSD for synthetic data "sim1"
sim1_dist <- function(a1,a2){
  res <- measure_distance(c(a1,a2),dat=sim1)
  return(res)
}

models <- models %>%
  mutate(RMSD = map2_dbl(a1,a2,sim1_dist))
print(models)

best_models <- models %>%
  filter(rank(RMSD) <= 10)
  
fnam <- "../results/modelling/synthetic3.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
sim1 %>%
  ggplot() +
    geom_point(aes(x,y),size=2,color="grey30") +
    geom_abline(data=best_models,aes(intercept=a1,slope=a2,color=-RMSD))
dev.off()

fnam <- "../results/modelling/synthetic4.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
models %>%
  ggplot() +
    geom_point(data=best_models,aes(a1,a2),size=4,color="red") +
    geom_point(aes(a1,a2,color=-RMSD))
dev.off()

#### 2.2. fit model (grid search)
models_grid <- expand.grid(
  a1 = seq(0,15,length=25),
  a2 = seq(2,5,length=25)) %>%
    mutate(RMSD=map2_dbl(a1,a2,sim1_dist))
head(models_grid,n=15)

best_grid <- models_grid %>%
  filter(rank(RMSD) <= 10)

fnam <- "../results/modelling/synthetic5.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
sim1 %>%
  ggplot() + 
    geom_point(aes(x,y),size=2,color="grey30") +
    geom_abline(data=best_grid,aes(intercept=a1,slope=a2,color=-RMSD))
dev.off()

fnam <- "../results/modelling/synthetic6.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
models_grid %>%
  ggplot() +
    geom_point(data=best_grid,aes(a1,a2),size=4,color="red") +
    geom_point(aes(a1,a2,color=-RMSD))
dev.off()

#### 2.3. fit model (Newton-Raphson optimization)
best_optim <- optim(c(0,0),measure_distance,dat=sim1)  
best_a1 <- best_optim$par[1]
best_a2 <- best_optim$par[2]

fnam <- "../results/modelling/synthetic7.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
sim1 %>%
  ggplot() + 
    geom_point(aes(x,y),size=2,color="grey30") +
    geom_abline(aes(intercept=best_a1,slope=best_a2))
dev.off()

#### 2.4. fit model (Least-Squares method)
best_lm <- lm(y ~ x,data=sim1)
best_a1 <- best_lm$coef[1]
best_a2 <- best_lm$coef[2]

fnam <- "../results/modelling/synthetic8.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
sim1 %>%
  ggplot() + 
    geom_point(aes(x,y),size=2,color="grey30") +
    geom_abline(aes(intercept=best_a1,slope=best_a2))
dev.off()

#### 2.5. summary
res1 <- models %>%
  filter(rank(RMSD) == 1)

res2 <- models_grid %>%
  filter(rank(RMSD) == 1)

res3 <- tibble(
  a1 = best_optim$par[1],
  a2 = best_optim$par[2],
  RMSD = best_optim$value)

res4 <- tibble(
  a1 = best_lm$coef[1],
  a2 = best_lm$coef[2],
  RMSD = sqrt(mean(best_lm$residuals^2)))

tab1 = round(data.frame(rbind(res1,res2,res3,res4)),digits=2)
rownames(tab1) = c("random","grid","NR","LS")
fnam = "../results/modelling/fitting1.txt"
write.table(tab1,fnam,sep=",",row.names=TRUE,col.names=NA)

## 3. Model Diagnostic
set.seed(12345)

#synthetic data
real_a1 <- 4.22
real_a2 <- 2.05
x <- round(runif(n=30,min=0,max=10),digits=2)
y <- real_a1*x + real_a2 + rnorm(n=30,mean=0,sd=1)
sim1 <- tibble(x,y)

#fit model
sim1_mod <- lm(y ~ x,data=sim1)

sim1 <- sim1 %>%
  add_predictions(sim1_mod) %>%
  add_residuals(sim1_mod)
print(sim1)

#scatterplot y~x  
fnam <- "../results/modelling/visualization1.tiff"
tiff(file=fnam,units="in",width=3,height=2.4,res=300,compression="lzw")
sim1 %>% 
  ggplot() + 
    geom_point(aes(x,y),size=2,color="grey30") +
    geom_line(aes(x,pred)) +
	geom_segment(aes(x=x,xend=x,y=y,yend=pred),alpha=0.2)
dev.off()

#scatterplot y~pred
fnam <- "../results/modelling/visualization2.tiff"
tiff(file=fnam,units="in",width=3,height=2.4,res=300,compression="lzw")
sim1 %>% 
  ggplot() + 
    geom_abline(aes(intercept=0,slope=1),size=2,color="white") +
    geom_point(aes(pred,y),size=2,color="grey30")
dev.off()

#density(residuals)
fnam <- "../results/modelling/visualization3.tiff"
tiff(file=fnam,units="in",width=3,height=2.4,res=300,compression="lzw")
sim1 %>% 
  ggplot() + 
    geom_density(aes(resid)) +
    stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1))
dev.off()

#scatterplot res~x
fnam <- "../results/modelling/visualization4.tiff"
tiff(file=fnam,units="in",width=3,height=2.4,res=300,compression="lzw")
sim1 %>% 
  ggplot() + 
    geom_hline(aes(yintercept=0),size=2,color="white") +
    geom_point(aes(x,resid))
dev.off()

#qqplot(residuals)
fnam <- "../results/modelling/visualization5.tiff"
tiff(file=fnam,units="in",width=3,height=2.4,res=300,compression="lzw")
sim1 %>% 
  ggplot() + 
    geom_qq_line(aes(sample=resid),size=2,color="white") + 
    geom_qq(aes(sample=resid)) + 
    labs(x="Theoretical Quantiles",y="Sample Quantiles") 
dev.off()

#lagplot(residuals)  
fnam <- "../results/modelling/visualization6.tiff"
tiff(file=fnam,units="in",width=3,height=2.4,res=300,compression="lzw")
sim1 %>% 
  ggplot() + 
    geom_point(aes(c(resid[-1],NA),resid)) +
    labs(x=expression(resid[i-1]),y=expression(resid[i]))
dev.off()
