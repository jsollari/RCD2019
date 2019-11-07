#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   3.6.1
#criado:     14.10.2019
#modificado: 27.10.2019

setwd("2019/2019.08.13_formacao_RCD2019/bin/")
library("tidyverse")

## 1. Scatterplots
?mpg

#table
print(mpg)

#scatterplot
fnam <- "../results/visualization/scatterplot1.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ,y=hwy))
dev.off()

#multiple scatterplot
fnam <- "../results/visualization/scatterplot2.tiff"
tiff(file=fnam,units="in",width=6,height=4,res=300,compression="lzw")
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ,y=hwy,color=class))
dev.off()

#smoothplot
fnam <- "../results/visualization/scatterplot3.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ,y=hwy))
dev.off()

#multiple smoothplot
fnam <- "../results/visualization/scatterplot4.tiff"
tiff(file=fnam,units="in",width=6,height=4,res=300,compression="lzw")
ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ,y=hwy,color=class))
dev.off()

## 2. Barplots
?diamonds
print(diamonds)

#barplot
fnam <- "../results/visualization/barplot1.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut))
dev.off()

#barplot (proportions)
fnam <- "../results/visualization/barplot2.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut,y=..prop..,group=1))
dev.off()

#stacked barplot
fnam <- "../results/visualization/barplot3.tiff"
tiff(file=fnam,units="in",width=6,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut,fill=clarity))
dev.off()

#stacked barplot (proportions)
fnam <- "../results/visualization/barplot4.tiff"
tiff(file=fnam,units="in",width=6,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut,fill=clarity),position="fill")
dev.off()

#clustered barplot
fnam <- "../results/visualization/barplot5.tiff"
tiff(file=fnam,units="in",width=6,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut,fill=clarity),position="dodge")
dev.off()

## 3. Boxplot
?diamonds
print(diamonds)

#boxplot 1
fnam <- "../results/visualization/boxplot1.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_boxplot(mapping=aes(y=carat))
dev.off()

#boxplot 2
fnam <- "../results/visualization/boxplot2.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_boxplot(mapping=aes(x=cut,y=carat))
dev.off()

## 4. Histograms
?diamonds
print(diamonds)

#histogram 1
fnam <- "../results/visualization/histogram1.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_histogram(mapping=aes(x=carat))
dev.off()

#histogram 2
fnam <- "../results/visualization/histogram2.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_histogram(mapping=aes(x=carat),binwidth=0.01)
dev.off()

#histogram 3
fnam <- "../results/visualization/histogram3.tiff"
tiff(file=fnam,units="in",width=5,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_freqpoly(mapping=aes(x=carat),binwidth=0.01)
dev.off()

#histogram 4
fnam <- "../results/visualization/histogram4.tiff"
tiff(file=fnam,units="in",width=6,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_freqpoly(mapping=aes(x=carat,color=cut),binwidth=0.1)
dev.off()

#histogram 5
fnam <- "../results/visualization/histogram5.tiff"
tiff(file=fnam,units="in",width=6,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_freqpoly(mapping=aes(x=carat,y=..density..,color=cut),binwidth=0.1)
dev.off()

#histogram 6
fnam <- "../results/visualization/histogram6.tiff"
tiff(file=fnam,units="in",width=6,height=4,res=300,compression="lzw")
ggplot(data=diamonds) +
  geom_freqpoly(mapping=aes(x=carat,y=..density..),binwidth=0.1) +
  geom_density(mapping=aes(x=carat),color="red")
dev.off()
