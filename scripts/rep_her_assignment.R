#Clear all data
remove(list = ls())

#Load libraries
library(tidyverse)
library(lme4)
library(ggplot2)

#Load data
data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTbQSYFND4l5105-jZmsEAnX2n1xHTOA80D2rRnctLmqLx5n3DwZhLai2V6uZmoHpUIC8soTQsnNI5i/pub?gid=0&single=true&output=csv") |>
  dplyr::select(Individual, Sex, Mass, Year) |>
  dplyr::filter(Sex == "1") |>
  dplyr::filter(Individual != 90402739) |>
  na.omit()
data$Mass <- as.numeric(data$Mass)


#Make the GLLM
plot(data$Mass)
m1 <- lmer(Mass ~ (1|Individual), data= data)

summary(m1)
repeatability1 <- 4526 / (4526 + 9787)
repeatability1 #0.316

m2 <- lmer(Mass ~ (1|Individual) + (1|Year), data= data)

summary(m2)
repeatability2 <- 3228 / (3228 + 2013 + 9145)
repeatability2 #0.224


herit <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToaxR52HV3eiUxu7jAWmT4DE_TRLJrLEOLHm0APmUN2dY1hCnnshFL7s117jiLqisTH-xAI16xS2Hr/pub?gid=1096002567&single=true&output=csv") |>
  dplyr::select(ChildID, MumID, BirthYear)

herit_child <- left_join(herit, data, by = c("ChildID" = "Individual")) |>
  na.omit()

h_mum <- herit |>
  dplyr::select(MumID)
herit_mum <- left_join(h_mum, data, by = c("MumID" = "Individual")) |>
  na.omit()

herit_all <- left_join(herit_child, herit_mum, by = "MumID") |>
  #rename(ChildMass = Mass.x, MumMass = Mass.y, ChildYear = BirthYear.x, MumYear = BirthYear.y) |>
  na.omit()

m3 <- lm(Mass.x ~ Mass.y, data= herit_all)
summary(m3)$coefficients




