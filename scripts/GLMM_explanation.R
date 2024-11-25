#Clear all data
remove(list = ls())

#Load libraries
library(tidyverse)
library(lme4)
library(ggplot2)

#Load data
CS <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQVcphvEPyp-VCzTdRQUtE_M4KkylBLS9yfzNGSCAtC08NrOhupLAZPMeQgxd4TLIK11JPhq9cMqV5p/pub?gid=1736259690&single=true&output=csv") |>
  na.omit(CS) |>
  as_tibble()

#Check for outliers
hist(CS$CS)


#Step 1: linear regression
m1 <- lm(CS ~ annual_density, data = CS)
summary(m1)

#Visualizing regression
ggplot(CS, aes(x = annual_density, y = CS)) +
  geom_point(shape=1) + #Use hollow circles
  geom_smooth(method = "lm", se = FALSE)

#Check residuals
par(mfrow=c(2,2))
plot(m1)
hist(residuals(m1))


#Step 2: linear regression - density as factor

CS$annual_density <- as.factor(CS$annual_density_cat)
m2 <- lm(CS ~ annual_density, data = CS)
summary(m2)


#Step 3: linear regression - taking annual means
CS2 <- CS |>
  dplyr::group_by(annual_density) |>
  dplyr::summarise(CS_avg = mean(CS),
                   CS_sd = sd(CS),
                   n_obs = n(),
                   CS_se = CS_sd/sqrt(n_obs))
m3 <- lm(CS_avg ~ annual_density, data = CS2)
summary(m3)


#GLMM modeling
library(lme4)

m4<-glmer(CS ~ annual_density+ (1 | plotID) + (1 | femaleID), data = CS, family = "gaussian")

summary(m4)
confint(m4)

m5<-glmer(CS ~ (1|femaleID), data= CS, family="gaussian")
summary(m5)
confint(m5)
repeatability5 <- 1.211 / (1.211 + 1.544) #1.211 is random effect variance and 1.544 is residual variance
repeatability5

m6<-glmer(CS ~ annual_density + (1|femaleID), data= CS, family="gaussian")
summary(m6)
confint(m6)

m7<-glmer(CS ~ annual_density + (1|femaleID)+ (1|year), data= CS, family="gaussian")
summary(m7)
confint(m7)




