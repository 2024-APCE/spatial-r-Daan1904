#### STRUCTURAL EQUATION MODELLING USING LAVAAN


# restore libraries
rm(list = ls()) # clear environment

library(tidyverse)
library(lavaan)

# Serengeti analysis on woody cover prediction

# dataset:

# browseURL("https://docs.google.com/spreadsheets/d/1lexN5NtKk9Z8xv-jk4slpHLAP_BkDE00kohABrIZJOI/edit?gid=452061108#gid=452061108")
# read the data from the google docs link:
SEMdata <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTPVA41JED_q7nrZmtsiU5U6FGjUya9FgRL4muSNTdvNmgfzqbgB7WpclghiuUHnyEoiyWHZgdrXcbT/pub?gid=452061108&single=true&output=csv")
names(SEMdata)


# standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% select(dist2river, elevation, rainfall, cec,
                                       burnfreq, hills, NDVI, woody),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd %>% select(dist2river, elevation, rainfall, cec,
                                          burnfreq, hills, NDVI, woody),
                    stars = T, ellipses = F)


# analyse the model (response ~ predictors) with a multiple regression approach
multreg_std <- lm(woody ~ dist2river + elevation + rainfall + cec + burnfreq +
                  hills + NDVI, data = SEMdatastd)
summary(multreg_std)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model and fit the model 
woody_model <- "woody ~ NDVI + cec + burnfreq + dist2river + rainfall
                dist2river ~ rainfall + hills
                burnfreq ~ rainfall
                cec ~ rainfall + burnfreq + dist2river
                NDVI ~ rainfall + cec + dist2river+ elevation
                rainfall ~ elevation
                elevation ~ hills"
woody_model

woody_fit <- lavaan::sem(woody_model, data = SEMdatastd)

# show the model results
summary(woody_fit, standardized = T, fit.measures = T, rsquare = T)
# goodness of fit (should be >0.9): CFI and TLI
# CFI = 0.911 / TLI = 0.793
# badness of fit: ( should be <0.1): RMSEA, SRMR
# RMSEA = 0.191 / SRMR = 0.047

<<<<<<< HEAD
# visualise the model
=======
  >>>>>>> 8a237fe2317acaad42b557f15ab08d729405ba65

# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper