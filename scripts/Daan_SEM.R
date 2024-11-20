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
psych::pairs.panels(SEMdata %>% dplyr::select(dist2river, elevation, rainfall, cec,
                                       burnfreq, hills, NDVI, woody),
                    stars = T, ellipses = F)
#Save the plot
#ggsave("C:/Users/daank/OneDrive - University of Twente/Documents/Github/APCE2024/spatial-r-Daan1904/plots/pairs_panel_SEM.png", width = 9.76, height = 8.34, dpi = 600)

psych::pairs.panels(SEMdatastd %>% dplyr::select(dist2river, elevation, rainfall, cec,
                                          burnfreq, hills, NDVI, woody),
                    stars = T, ellipses = F)


# analyse the model (response ~ predictors) with a multiple regression approach
multreg_std <- lm(woody ~ dist2river + elevation + rainfall + cec + burnfreq +
                  hills + NDVI, data = SEMdatastd)
summary(multreg_std)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model and fit the model
# Model 1 - CEC influenced by rivers
woody_model1 <- "woody ~ NDVI + cec + burnfreq + dist2river + rainfall
                dist2river ~ rainfall + hills
                burnfreq ~ rainfall
                cec ~ rainfall + burnfreq + dist2river
                NDVI ~ rainfall + cec + dist2river+ elevation
                rainfall ~ elevation
                elevation ~ hills"

# Model 2 - CEC influenced by elevation
woody_model2 <- "woody ~ NDVI + cec + burnfreq + dist2river + rainfall
                dist2river ~ rainfall + hills
                burnfreq ~ rainfall
                cec ~ elevation
                NDVI ~ rainfall + cec + dist2river+ elevation
                rainfall ~ elevation
                elevation ~ hills"

# Model 3 - CEC influenced by elevation and no burnfreq
woody_model3 <- "woody ~ NDVI + cec + dist2river + rainfall
                dist2river ~ elevation + hills
                cec ~ elevation
                NDVI ~ rainfall + cec + dist2river+ elevation
                rainfall ~ elevation
                elevation ~~ hills"

woody_model2
woody_fit1 <- lavaan::sem(woody_model1, data = SEMdatastd)
woody_fit2 <- lavaan::sem(woody_model2, data = SEMdatastd)
woody_fit3 <- lavaan::sem(woody_model3, data = SEMdatastd)

# show the model results
summary(woody_fit3, standardized = T, fit.measures = T, rsquare = T)
# goodness of fit (should be >0.9): CFI and TLI
# CFI = 0.911 / TLI = 0.793
# badness of fit: ( should be <0.1): RMSEA, SRMR
# RMSEA = 0.191 / SRMR = 0.047

