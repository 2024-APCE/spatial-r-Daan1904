# Piecewise SEM

library(piecewiseSEM)

# read the pointdata
pointdata_init<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTPVA41JED_q7nrZmtsiU5U6FGjUya9FgRL4muSNTdvNmgfzqbgB7WpclghiuUHnyEoiyWHZgdrXcbT/pub?gid=452061108&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr::select(woody, cec, NDVI, rainfall, dist2river, elevation, hills) |>  # select the variables of interest
  dplyr:: filter(woody<75)   # remove 2 extreme values
pointdata

# note that you should not standardize your data for a Piecewise SEM as then e.g. logistic regression cannot be used

# Check for missing values
sum(is.na(pointdata))
colSums(is.na(pointdata))


psych::pairs.panels(pointdata,stars = T, ellipses = F)


# Define the models

# model_woody: woody predicted by cec, NDVI, rainfall and dist2river
# Good labeling is to use the variable in the name which is predicted
model_woody <- glm(woody ~  cec + NDVI + rainfall + dist2river + elevation,
                  data = pointdata)
summary(model_woody)
p1<-ggplot(data=pointdata,aes(x=cec,y=woody))+
  geom_point() +
  geom_smooth(method="glm",
              formula= y~x,
              se=T)
p1
p2<-ggplot(data=pointdata,aes(x=NDVI,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
p2
p3<-ggplot(data=pointdata,aes(x=rainfall,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p3
p4<-ggplot(data=pointdata,aes(x=dist2river,y=woody))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p4

# model_cec: predicted by rainfall
model_cec <- lm(cec ~ elevation, 
                data = pointdata)
summary(model_cec)

p5<-ggplot(data=pointdata,aes(y=cec,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p5

# model_NDVI:  predicted by cec, elevation, rainfall and dist2river
model_NDVI <-lm(NDVI ~ cec + elevation + rainfall + dist2river,
                      data=pointdata)
summary(model_NDVI)
p6<-ggplot(data=pointdata,aes(x=elevation, y=NDVI))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~poly(x,2),
              se=T)
p6
p7<-ggplot(data=pointdata,aes(y=NDVI,x=cec))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p7
p8<-ggplot(data=pointdata,aes(y=NDVI,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p8
p9<-ggplot(data=pointdata,aes(y=NDVI,x=dist2river))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p9

# model_rainfall: rainfall predicted by elevation
model_rainfall <- lm(rainfall ~ elevation, 
                     data = pointdata)
summary(model_rainfall)

p10<-ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y ~ x,
              se=T)
p10

# model_dist2river: dist2river predicted by elevation and hills
model_dist2river <- lm(dist2river ~ elevation + hills + cec, 
                       data = pointdata)
summary(model_dist2river)

p11<-ggplot(data=pointdata,aes(y=dist2river,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y ~ x,
              se=T)
p11
p12<-ggplot(data=pointdata,aes(y=dist2river,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y ~ x,
              se=T)
p12

# model_elev_hills: correlation between elevation and hills
model_elev_hills <- glm(hills ~ elevation, family = binomial,
                       data = pointdata)
summary(model_elev_hills)

p13<-ggplot(data=pointdata,aes(y=hills ,x=elevation))+
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"))
p13

# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_cec,
                                 model_NDVI,
                                 model_rainfall,
                                 model_dist2river,
                                 model_elev_hills)

# Summarize the SEM results
summary(psem_model, conserve = T)



# a Significant (P<0.05) global goodness of fit means that your model does not fit well, 
# indicating potential problems like missing paths, mis-specfied relations, 
# or unaccounted-for correlations

# update the model based on your results
# significant tests of directed separation could mean a missing direct effect between the variables

# Best Practices:
# - Hypothesize Carefully:
#   Construct the initial model based on theoretical or empirical understanding.
# - Evaluate directed Separation Results:
#   Add or adjust paths based on significant independence test results.
# - Refit and Validate:
#   Rerun the model after adjustments and recheck the Fisher’s C statistic and independence claims.
# - Avoid Overfitting:
#   Add paths only when justified by theory or strong evidence, not purely to improve fit.
# Common pitfall: 
# - ignofing significant d-separation tests and failing to modify the model
# - adding too many variables and pathways, leading to overfitting and loss of parsimony