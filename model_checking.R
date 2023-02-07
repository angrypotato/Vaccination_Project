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
clinic.gam.form <- as.formula(TotalClinicsCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) +  s(night_lights, k=5) +
                         s(Population, k=5) + s(child_population, k=5) + s(population_density, k=5) +
                         s(radio, k=5)+ s(electricity, k=5)+ s(television, k=5) + s(mobile_phone, k=5) + s(mothers_age, k=5))

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
par(mfrow = c(4,3))
clinic_plot <-plot(clinic_gam_model)

## poverty
clinic_poverty <- clinic_plot[3][[1]]
x <- clinic_poverty$x
y <- clinic_poverty$fit # predicted output of s(poverty)
plot(x, y)
lm(y ~ x)

# adjust model validation process

## use the whole dataset to build model
clinic.complete <- scale(tehsils.clinic) %>%
  as.data.frame()
clinic_gam_complete <- gam(clinic.gam.form, data = clinic.complete, method = "REML") 
par(mfrow = c(2, 2))
gam.check(clinic_gam_complete, k.rep = 500) 
par(mfrow = c(4,3))
clinic_plot <-plot(clinic_gam_complete)

## repeated k-fold CV (to be edited)
set.seed(0)

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, "night_lights"=NA, "Population"=NA,"child_population"=NA,"population_density"=NA, 
                    "radio"=NA,  "electricity"=NA, "television"=NA,"mobile_phone"=NA, "mothers_age"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  gam.mod <- gam(gam.form, data = sample_d, method = "REML") 
  
  raw_pred = predict(gam.mod,pentaTest)
  preds <- raw_pred*train.sd[20]+train.mean[20]
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  
  ## fill in the blank list
  
  clinic_gam_summary <- summary(gam.mod$finalModel)
  clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  clinic_gam_cfs  <- as.data.frame(t(clinic_gam_cfs))
  names(clinic_gam_cfs) <- c("fertility","poverty","population_density", 
                             "radio",  "electricity", "antenatal_care","mothers_age")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  
  print(i)
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame( "fertility"=c(mean(coef_clean$fertility),std_mean(coef_clean$fertility)), 
                          "poverty"=c(mean(coef_clean$poverty), std_mean(coef_clean$poverty)),
                          "population_density"=c(mean(coef_clean$population_density), std_mean(coef_clean$population_density)), 
                          "radio"=c(mean(coef_clean$radio),std_mean(coef_clean$radio)),  
                          "electricity"=c(mean(coef_clean$electricity), std_mean(coef_clean$electricity)),
                          "antenatal_care"=c(mean(coef_clean$antenatal_care),std_mean(coef_clean$antenatal_care)), 
                          "mothers_age"=c(mean(coef_clean$mothers_age),std_mean(coef_clean$mothers_age)))


mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
View(t(coef_final))



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
ratio.gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + s(night_lights, k=5) +
                               + s(Population, k=5) + s(child_population, k=5) + s(population_density,k=5) +s(radio, k=5) + s(electricity, k=5)  
                               + s(mobile_phone, k=5) + s(antenatal_care, k=5) + s(mothers_age, k=5))

ratio_gam_model <- gam(ratio.gam.form, data = ratio.pentaTrain, method = "REML") 
par(mfrow = c(2,2))
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
ratio_plot <- plot(ratio_gam_model)
# poverty
ratio_poverty <- ratio_plot[3][[1]]
x <- ratio_poverty$x
y <- ratio_poverty$fit # predicted output of s(poverty)
par(mfrow=c(1,1))
plot(x, y)
lm(y ~ x)


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


# transformation to meet linearity
# on the raw df

# y ~ log(x); log(y) ~ log(x)
clinic_y <- tehsils.clinic$TotalClinicsCoverage
clinic_x <- data.matrix(tehsils.clinic[, c(10, 8, 12, 4, 1, 2, 11, 9, 7, 16, 3, 13)])
par(mfrow = c(3,3))
for (i in 10:12) {
  plot(clinic_y ~ clinic_x[,i])
  title(main = colnames(clinic_x)[i])
  plot(clinic_y ~ log(clinic_x[,i]))
  title(main = paste0("log(",colnames(clinic_x)[i],")"))
  plot(log(clinic_y) ~ log(clinic_x[,i]))
  title(main = paste0("log(y) ~ log(",colnames(clinic_x)[i],")"))
}

# log(y) ~ x; sqrt(y) ~ x; 1/y ~ x
par(mfrow = c(3,4))
for (i in 10:12) {
  plot(clinic_y ~ clinic_x[,i])
  title(main = colnames(clinic_x)[i])
  plot(log(clinic_y) ~ clinic_x[,i])
  title(main = paste0("log(y) ~ ",colnames(clinic_x)[i]))
  plot(sqrt(clinic_y) ~ clinic_x[,i])
  title(main = paste0("sqrt(y) ~ ",colnames(clinic_x)[i]))
  plot(1/clinic_y ~ clinic_x[,i])
  title(main = paste0("1/y ~ ",colnames(clinic_x)[i]))
}

# log(y) ~ log(x)
## ggplot
clinic_df <- tehsils.clinic[, c(10, 8, 12, 4, 1, 2, 11, 9, 7, 16, 3, 13, 20)]
library(ggpubr)
for (i in 1:12) {
  raw <- ggplot(data = clinic_df, aes_string(x = colnames(clinic_df)[i], y = "TotalClinicsCoverage")) +
    geom_point() +
    ggtitle(colnames(clinic_df)[i])
  log <- ggplot(data = clinic_df, aes_string(x = colnames(clinic_df)[i], y = "TotalClinicsCoverage")) +
    geom_point() +
    scale_x_log10() + scale_y_log10() +
    ggtitle(colnames(clinic_df)[i])
  print(ggarrange(raw, log, ncol = 2, nrow = 1,labels = c("raw", "log")))
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
