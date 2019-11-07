#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     21.10.2019
#modificado: 04.11.2019

setwd("2019/2019.08.13_formacao_RCD2019/bin/")
library("tidyverse")

#1.1.
?geom_freqpoly
?geom_density
?geom_boxplot
?geom_histogram

#1.2.
ggplot(data=mpg)

#1.3.
print(mpg)
dim(mpg)
str(mpg)
nrow(mpg)
ncol(mpg)

#1.4.
?mpg

#1.5.
?mpg

#1.6. 
ggplot(data=mpg) + 
  geom_point(mapping=aes(x=cyl,y=hwy))
ggplot(data=mpg) + 
  geom_boxplot(mapping=aes(x=as.factor(cyl),y=hwy))
library("hexbin")
ggplot(data=mpg) + 
  geom_hex(mapping=aes(x=cyl,y=hwy))

#1.7.
print(diamonds)
dim(diamonds)
str(diamonds)
nrow(diamonds)
ncol(diamonds)

#1.8.
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut,fill=color),position="fill")

#1.9.
ggplot(data=diamonds) +
  geom_boxplot(mapping=aes(x=color,y=price))
ggplot(data=diamonds) +
  geom_freqpoly(mapping=aes(x=price,y=..density..,color=color),binwidth=500)  

