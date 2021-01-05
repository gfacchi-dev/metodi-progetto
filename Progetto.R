rm(list = ls())
#Require Package per la visualizzazione dei missing values
require(Amelia)
df <- read.csv("insurance.csv")
str(df)
head(df)

levels(df$smoker) <- c(0, 1)
head(df)

missmap(df)


library(MASS)
library(plyr)
library(ggplot2)
library(knitr)
library(GGally)

kable(summary(df))
#Verifica di collinearità
pairs(df)
#Correlazione tra anno e età = -1
plot(charges ~ smoker, data=df)
plot(charges ~ bmi, data=df)
boxplot(x = df$smoker)
