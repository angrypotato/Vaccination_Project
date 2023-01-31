##### Model checking for GAM and Ridge models using in_clinic, outreach, and ratio data

# Import packages and data ----

## packages and functions -----
source(file='PreRunNew.r')
library(genridge)
library(Boruta)
library(mgcv)
library(glmnet)

std_mean <- function(x) sd(x)/sqrt(length(x))

test_scale <- function(raw_test, train.mean, train.sd) {
  df <- raw_test
  for (n in 1:ncol(df)) {
    df[,n] <- (raw_test[,n] - train.mean[n])/train.sd[n]
  }
  df
}

adj.r2 <- function(r2, n, p) {
  1 - (1-r2)*(n-1)/(n-p-1)
}

## data ----
tehsils <- read.csv("results/tehsils_complete_buffer12_9.27.csv")

tehsils.clinic <- tehsils[,c(3:17,19,21:23,28)]
tehsils.clinic <- tehsils.clinic[complete.cases(tehsils.clinic), ]  
set.seed(0)
clinic_data_split = sample.split(tehsils.clinic, SplitRatio = 0.8)
clinic.pentaTrain <- subset(tehsils.clinic, clinic_data_split == TRUE) %>%
  scale() %>%
  as.data.frame()

tehsils.outreach <- tehsils[,c(3:17,19,21:23,27)]
tehsils.outreach <- tehsils.outreach[complete.cases(tehsils.outreach),]
set.seed(43)
outreach_data_split = sample.split(tehsils.outreach, SplitRatio = 0.8)
outreach.pentaTrain <- subset(tehsils.outreach, outreach_data_split == TRUE) %>%
  scale() %>%
  as.data.frame()

tehsils.ratio <- tehsils[,c(3:17,19,21:23,26)] 
tehsils.ratio <- tehsils.ratio[complete.cases(tehsils.ratio), ]
set.seed(1)
ratio_data_split = sample.split(tehsils.ratio, SplitRatio = 0.8)
ratio.pentaTrain <- subset(tehsils.ratio, ratio_data_split == TRUE) %>%
  scale() %>%
  as.data.frame()


# Model checking ----

## GAM ----
## check df and homoscedasticity

### in clinic ----
clinic.gam.form <- as.formula(TotalClinicsCoverage ~ s(fertility, k=5) + s(elevation, k=5)  +  s(night_lights, k=5) +
                         s(Population, k=5) + s(child_population, k=5) +  s(mobile_phone, k=5) +
                         s(radio, k=5)+ s(electricity, k=5)+ s(television, k=5)  + s(mothers_age, k=5))

set.seed(0)

# using the whole train set
clinic_gam_model <- gam(clinic.gam.form, data = clinic.pentaTrain, method = "REML") 
par(mfrow = c(2, 2))
gam.check(clinic_gam_model, k.rep = 500) 

# using bootstrapped data, get low p-value
for (i in 1:5) {
  sample_d = clinic.pentaTrain[sample(1:nrow(clinic.pentaTrain), nrow(clinic.pentaTrain), replace = TRUE), ]

  clinic_gam_model <- gam(clinic.gam.form, data = sample_d, method = "REML") 
  
  gam.check(clinic_gam_model, k.rep = 500) # how to save the plots?

  print(i)
}

# plotting the relationship
summary(clinic_gam_model)
model_matrix <- predict(clinic_gam_model, type = "lpmatrix")
model_matrix
plot(clinic.pentaTrain$TotalClinicsCoverage ~ clinic.pentaTrain$fertility)
abline(h = 0)
lines(clinic.pentaTrain$fertility, model_matrix[, "s(fertility).1"], type = "l", lty = 2)
lines(clinic.pentaTrain$night_lights, model_matrix[, "s(night_lights).2"], type = "l", lty = 2)
lines(clinic.pentaTrain$night_lights, model_matrix[, "s(night_lights).3"], type = "l", lty = 2)
lines(clinic.pentaTrain$night_lights, model_matrix[, "s(night_lights).4"], type = "l", lty = 2)

ggplot(clinic.pentaTrain, aes(fertility, TotalClinicsCoverage)) +
  geom_point() +
  geom_smooth(method = "gam", formula = clinic.pentaTrain$TotalClinicsCoverage ~ s(clinic.pentaTrain$fertility, k=5))

par(mfrow = c(4,3))
plot(clinic_gam_model)

gam_pred <- predict(clinic_gam_model)
ggplot(CO2_pred, aes(x = time)) +
  geom_point(aes(y = co2), size = 1, alpha = 0.5) +
  geom_line(aes(y = predicted_values), colour = "red")


### outreach ----
outreach.gam.form <- as.formula(TotalOutreachCoverage ~ s(night_lights, k=5) + s(elevation, k=5) +
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + 
                         s(antenatal_care, k=5) + s(mothers_age, k=5))

outreach_gam_model <- gam(outreach.gam.form, data = outreach.pentaTrain, method = "REML") 
gam.check(outreach_gam_model, k.rep = 500) 

# using bootstrapped data
set.seed(0)
for (i in 1:5) {
  sample_d = outreach.pentaTrain[sample(1:nrow(outreach.pentaTrain), nrow(outreach.pentaTrain), replace = TRUE), ]
  
  outreach_gam_model <- gam(outreach.gam.form, data = sample_d, method = "REML") 
  
  gam.check(outreach_gam_model, k.rep = 500) # how to save the plots?
  
  print(i)
}

# plot
summary(outreach_gam_model)
par(mfrow = c(4,2))
plot(outreach_gam_model)



### ratio ----
ratio.gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5)   + s(poverty, k=5)  + s(elevation, k=5) +
                               s(population_density,k=5) +s(radio, k=5) + s(electricity, k=5)  + 
                               s(antenatal_care, k=5) + s(mothers_age, k=5) + s(child_population, k=5) +
                               s(Population, k=5) + s(night_lights, k=5))

ratio_gam_model <- gam(ratio.gam.form, data = ratio.pentaTrain, method = "REML") 
gam.check(ratio_gam_model, k.rep = 500) 

# using bootstrapped data
set.seed(0)
for (i in 1:5) {
  sample_d = ratio.pentaTrain[sample(1:nrow(ratio.pentaTrain), nrow(ratio.pentaTrain), replace = TRUE), ]
  
  ratio_gam_model <- gam(ratio.gam.form, data = sample_d, method = "REML") 
  
  gam.check(ratio_gam_model, k.rep = 500) # how to save the plots?
  
  print(i)
}

# plot
summary(ratio_gam_model)
par(mfrow = c(4,3))
plot(ratio_gam_model)



## Rigde ----

### in clinic ----
clinic_y <- clinic.pentaTrain$TotalClinicsCoverage
clinic_x <- data.matrix(clinic.pentaTrain[, c(10, 8, 12, 4, 1, 2, 11, 9, 7, 16, 3, 13)])

clinic_ridge_model <- cv.glmnet(clinic_x, clinic_y, alpha = 0, standardize = F)

clinic_best_lambda <- clinic_ridge_model$lambda.min

clinic_best_model <- glmnet(clinic_x, clinic_y, alpha = 0, lambda = clinic_best_lambda, standardize = F)

# assumption of linearity is not met
par(mfrow =c(4,3))
for (i in 1:12) {
  plot(clinic_y ~ clinic_x[,i], )
  title(main = colnames(clinic_x)[i])
}



### outreach ----
outreach_y <- outreach.pentaTrain$TotalOutreachCoverage
outreach_x <- data.matrix(outreach.pentaTrain[, c(8,6,4,7,15,2,16)])

par(mfrow =c(4,2))
for (i in 1:7) {
  plot(outreach_y ~ outreach_x[,i], )
  title(main = colnames(outreach_x)[i])
}


### ratio ----
ratio_y <- ratio.pentaTrain$OutreachProportion
ratio_x <- data.matrix(ratio.pentaTrain[, c(10,15,9,3,16,2,8,11,4,13,1)])

par(mfrow =c(4,3))
for (i in 1:11) {
  plot(ratio_y ~ ratio_x[,i], )
  title(main = colnames(ratio_x)[i])
}
