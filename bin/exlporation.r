#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     16.10.2019
#modificado: 05.11.2019

setwd("2019/2019.08.13_formacao_RCD2019/bin/")
library("tidyverse")

## 1. Outliers

#table
?diamonds
print(diamonds)

#summary
diamonds %>%
  summary()

#plots
fnam <- "../results/exploration/outliers1.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_histogram(mapping=aes(x=x),binwidth=0.5)
dev.off()

fnam <- "../results/exploration/outliers2.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_histogram(mapping=aes(x=y),binwidth=0.5)
dev.off()

fnam <- "../results/exploration/outliers3.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_histogram(mapping=aes(x=z),binwidth=0.5)
dev.off()

fnam <- "../results/exploration/outliers4.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_histogram(mapping=aes(x=x),binwidth=0.5) + 
    coord_cartesian(ylim=c(0,50))
dev.off()

fnam <- "../results/exploration/outliers5.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_histogram(mapping=aes(x=y),binwidth=0.5) + 
    coord_cartesian(ylim=c(0,50))
dev.off()

fnam <- "../results/exploration/outliers6.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_histogram(mapping=aes(x=z),binwidth=0.5) + 
    coord_cartesian(ylim=c(0,50))
dev.off()

#table
unusual <- diamonds %>%
  filter((x < 1) | (y < 1 | y > 20) | (z < 1 | z > 10)) %>%
  arrange(x)
print(unusual,n=23)

#plots
fnam <- "../results/exploration/outliers7.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_point(mapping=aes(x=price,y=x)) + 
    geom_point(data=unusual,mapping=aes(x=price,y=x),color="red")
dev.off()

fnam <- "../results/exploration/outliers8.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_point(mapping=aes(x=price,y=y)) + 
    geom_point(data=unusual,mapping=aes(x=price,y=y),color="red")
dev.off()

fnam <- "../results/exploration/outliers9.tiff"
tiff(file=fnam,units="in",width=3,height=3,res=300,compression="lzw")
diamonds %>%
  ggplot() +
    geom_point(mapping=aes(x=price,y=z)) + 
    geom_point(data=unusual,mapping=aes(x=price,y=z),color="red")
dev.off()

#summary
diamonds %>%
  filter((x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10)) %>%
  summary()

## 2. Missing values

#table
?diamonds
diamonds_NA <- diamonds %>%
  mutate(x = ifelse(x < 1,NA,x)) %>%
  mutate(y = ifelse(y < 1 | y > 20,NA,y)) %>%
  mutate(z = ifelse(z < 1 | z > 10,NA,z))
print(diamonds_NA)

#summary
diamonds_NA %>%
  group_by(cut) %>%
  summarize(mean_x = mean(x),
            mean_y = mean(y),
            mean_z = mean(z),
            n = n(),
            x_NA = sum(is.na(x)),
            y_NA = sum(is.na(y)),
            z_NA = sum(is.na(z)))

diamonds_NA %>%
  group_by(cut) %>%
  summarize(mean_x = mean(x,na.rm=TRUE),
            mean_y = mean(y,na.rm=TRUE),
            mean_z = mean(z,na.rm=TRUE),
            n = n(),
            x_NA = sum(is.na(x)),
            y_NA = sum(is.na(y)),
            z_NA = sum(is.na(z)))

#plots
diamonds_NA %>%
  ggplot() +
    geom_point(mapping=aes(x=price,y=x))

diamonds_NA %>%
  ggplot() +
    geom_point(mapping=aes(x=price,y=x),na.rm=TRUE)

## 3. Distribution (1D)
library("moments")

#table
?diamonds
diamonds2 <- diamonds %>%
  filter((x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10))
print(diamonds2)

#distribution 1
fnam <- "../results/exploration/distribution1D_1.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>% 
  ggplot() +
    geom_histogram(mapping=aes(x=price),binwidth=50)
dev.off()

#distribution 2
ldiamonds2 <- diamonds2 %>%
  mutate(price = log(price))
fnam <- "../results/exploration/distribution1D_2.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
ldiamonds2 %>% 
  ggplot() +
    geom_histogram(mapping=aes(x=price),binwidth=0.01)
dev.off()

#distribution 1 vs distribution 2
param <- diamonds2 %>%
  summarize(n = n(),
            mean = mean(price),
            sd = sd(price),
            median = median(price),
            IQR = IQR(price))
lparam <- ldiamonds2 %>% 
  summarize(n = n(),
            mean = mean(price),
            sd = sd(price),
            median = median(price),
            IQR = IQR(price))
tab1 <- round(cbind(t(param),t(lparam)),digits=2)
colnames(tab1) <- c("original","log()")
fnam = "../results/exploration/distribution1D_1.txt"
write.table(tab1,fnam,sep=",",row.names=TRUE,col.names=NA)

#distribution 3
ldiamonds2_scale <- ldiamonds2 %>%
  mutate(price = (price - mean(price))/sd(price))
fnam <- "../results/exploration/distribution1D_3.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
ldiamonds2_scale %>% 
  ggplot() +
    geom_histogram(mapping=aes(x=price,y=..density..),binwidth=0.2) +
    stat_function(fun=dnorm,color="red",args=list(mean=0,sd=1))
dev.off()

#distribution 4
price_mean <- mean(ldiamonds2$price)
price_sd <- sd(ldiamonds2$price)
fnam <- "../results/exploration/distribution1D_4.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
ldiamonds2 %>% 
  ggplot() +
    geom_histogram(mapping=aes(x=price,y=..density..),binwidth=0.2) +
    stat_function(fun=dnorm,color="red",args=list(mean=price_mean,sd=price_sd))
dev.off()

#distribution 3 vs Gaussian
param <- ldiamonds2_scale %>%
  summarize(n= n(),
            mean = mean(price),
            sd = sd(price),
            skewness = skewness(price),
            kurtosis = kurtosis(price))
tab2 <- round(cbind(t(param),c(1/0,0,1,0,3)),digits=3)
colnames(tab2) <- c("original","Gaussian")
fnam = "../results/exploration/distribution1D_2.txt"
write.table(tab2,fnam,sep=",",row.names=TRUE,col.names=NA)

## 4. Distribution (2D)

#table
?diamonds
diamonds2 <- diamonds %>%
  filter((x > 0) & (y > 0 & y < 20) & (z > 0 & z < 10))
print(diamonds2)

#### 4.1. Continuous vs categorical

#plots
fnam <- "../results/exploration/distribution2D_1.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  filter(cut=="Ideal") %>%
  ggplot() +
    geom_boxplot(mapping=aes(x=clarity,y=price))
dev.off()

#summary
diamonds2 %>%
  filter(cut=="Ideal") %>%
  group_by(clarity) %>%
  summarize(mean_price = mean(price))

#plots
fnam <- "../results/exploration/distribution2D_2.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  filter(cut=="Ideal") %>%
  group_by(clarity) %>%
  mutate(mean_price = mean(price)) %>%
  ggplot() +
    geom_point(mapping=aes(x=mean_price,y=price),size=2,alpha=0.01) + 
    geom_point(mapping=aes(x=mean_price,y=mean_price),size=2,color="red")
dev.off()

#correlation metric (correlation between observed and predicted values)
diamonds2 %>%
  filter(cut=="Ideal") %>%
  group_by(clarity) %>%
  mutate(mean_price = mean(price)) %>%
  ungroup() %>%
  summarize(cor(mean_price,price))

#### 4.2. Two categorical

#plots
fnam <- "../results/exploration/distribution2D_3.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_count(mapping=aes(x=clarity,y=cut))
dev.off()

fnam <- "../results/exploration/distribution2D_4.tiff"
tiff(file=fnam,units="in",width=4.5,height=3.5,res=300,compression="lzw")
diamonds2 %>%
  group_by(clarity,cut) %>%
  summarize(n = n()) %>%
  ggplot() +
    geom_count(mapping=aes(x=clarity,y=cut,color=n,size=n))
dev.off()

fnam <- "../results/exploration/distribution2D_5.tiff"
tiff(file=fnam,units="in",width=4.5,height=3.5,res=300,compression="lzw")
diamonds2 %>%
  group_by(clarity,cut) %>%
  summarize(n = n()) %>%
  ggplot() +
    geom_tile(mapping=aes(x=clarity,y=cut,fill=n))
dev.off()

#correlation metric (Cramer's V)

#Calculate Cramer's V from contingency table  
calc_Cramers_V <- function(x){
    chi_stat <- chisq.test(x)$statistic #chi-sqrt
    n <- sum(x)                         #sample size
    min_dim <- min(dim(x)) - 1          #minimum number of dimensions - 1 
    res <- sqrt(chi_stat/n/min_dim)     #Cramer's V
    names(res) = "Cramers.V"
    return(res)
}
cont_table <- table(diamonds2$clarity,diamonds2$cut) #contingency table
print(cont_table)
round(calc_Cramers_V(cont_table),digits=3)

#### 4.3. Two continuous
library("hexbin")

#plots
fnam <- "../results/exploration/distribution2D_5.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_point(mapping=aes(x=carat,y=price))
dev.off()

fnam <- "../results/exploration/distribution2D_6.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_point(mapping=aes(x=carat,y=price),alpha=0.01)
dev.off()
  
fnam <- "../results/exploration/distribution2D_7.tiff"
tiff(file=fnam,units="in",width=4,height=3,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_bin2d(mapping=aes(x=carat,y=price),bins=100)
dev.off()
  
fnam <- "../results/exploration/distribution2D_8.tiff"
tiff(file=fnam,units="in",width=4,height=3,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_hex(mapping=aes(x=carat,y=price),bins=100)
dev.off()

fnam <- "../results/exploration/distribution2D_9.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_boxplot(mapping=aes(x=carat,y=price,group=cut_width(carat,0.1)))
dev.off()

fnam <- "../results/exploration/distribution2D_10.tiff"
tiff(file=fnam,units="in",width=4,height=4,res=300,compression="lzw")
diamonds2 %>%
  ggplot() +
    geom_boxplot(mapping=aes(x=carat,y=price,group=cut_number(carat,20)))
dev.off()

#correlation metric
diamonds2 %>%
  summarize(cor(carat,price))
  