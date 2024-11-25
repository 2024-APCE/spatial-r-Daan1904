#Clear all data
remove(list = ls())

#Load libraries
library(tidyverse)
library(lme4)
library(ggplot2)

#Load data
CS_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQVcphvEPyp-VCzTdRQUtE_M4KkylBLS9yfzNGSCAtC08NrOhupLAZPMeQgxd4TLIK11JPhq9cMqV5p/pub?gid=1736259690&single=true&output=csv") |>
  na.omit(CS) |>
  as_tibble()

#Load package qdapTools to be able to use the lookup function
install.packages("qdapTools")
library(qdapTools)

#Center Annual Density  per individual
ind_avgF<-aggregate(cbind(annual_density)~femaleID,CS_data,mean) # Calc avg density per fem

## Between individual effect: mean density for each female! This is how individuals differ
CS_data$Btw_Ind_Dens<-lookup(CS_data$femaleID,ind_avgF[,c("femaleID","annual_density")])

## Within individual effect: how each value differs from individual mean.
CS_data$Wthin_Ind_Dens<-CS_data$annual_density-CS_data$Btw_Ind_Dens 

#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect
m6<-glmer(CS~Wthin_Ind_Dens + Btw_Ind_Dens+ (1|femaleID), data= CS_data, family="gaussian")
summary(m6)
confint(m6)







