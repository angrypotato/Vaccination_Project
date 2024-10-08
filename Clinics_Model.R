
# Purpose: 

# 1. Find Which Features Are the Most Significant in Predicting 
# the Ratio of Outreach Pentavalent Vaccinations to In Clinic Pentavalent Vaccination By Tehsil. 
# This is to say What features predict that more outreach vaccinations will need to be administered in 
# an area vs. those administered at clinics?
# 2.  Develop Models Using Such Covariates in Order to Predict this Ratio of Outreach / In Clinic 
# Pentavalent Vaccinations - Feature selection using RFE, Boruta.  Predictive Model using GBM, Lasso and GAM.  

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



# For Tehsil ----

### Split Data

### Take the existing Tehsil level data with covariates and Vaccination ratios and parse out the 
### covariates from the Y (Outreach/Clinic Vaccination Ratio)

tehsils <- read.csv("results/tehsils_complete_buffer12_9.27.csv")
tehsils.clinic <- tehsils[,c(3:17,19,21:23,28)] 

tehsils.clinic <- tehsils.clinic[complete.cases(tehsils.clinic), ]


### Split into train and test set

set.seed(1)
data_split = sample.split(tehsils.clinic, SplitRatio = 0.8)

pentaTrain.raw <- subset(tehsils.clinic, data_split == TRUE) 
train.sd <- apply(pentaTrain.raw, 2, sd)
train.mean <- apply(pentaTrain.raw, 2, mean)

pentaTrain <- scale(pentaTrain.raw) %>%
  as.data.frame()

pentaTest.raw <-subset(tehsils.clinic, data_split == FALSE) 

pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)


## Feature Selection ----

### RFE ----

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:19], pentaTrain[,20],sizes=c(1:19), rfeControl=rfcontrol)

# Summarize the results

print(results)
predictors(results)

### Boruta ----

library(Boruta)
set.seed(1)

boruta_output <- Boruta(OutreachProportion ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

## Plot signficance of covariates in predicting Y and then determine which are listed as confirmed, tentative or rejected from being significant

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)

### Those covariates that were determined 
### as confirmed or tentatively significant by the Boruta Models along with those that 
### were deemed as signfiicant by the RFE featire selection should be those included in modeling
### c(1:4,7:13,16)


## GBM ----

### feature pruning ----

clinic.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:3,8:11,13,15,16),
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(clinic.step,pentaTrain)
R2(pentaTrain[,20],gbm_pred)
adj.r2(R2(pentaTrain[,20],gbm_pred),96,10)

gbm_cfs <- summary(clinic.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))


### SE ----
set.seed(0)

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, "night_lights"=NA,"Population"=NA,
                    "child_population"=NA, "population_density"=NA, "radio"=NA, "electricity"=NA,
                    "television"=NA,"mobile_phone"=NA, "mothers_age"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x =  c(1:4,7:13,16),   # selected features 
    gbm.y = 20,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.005,
    bag.fraction = 0.5,
    cv_folds = 5,
    plot.main = F,
    verbose = F
  )
  
  raw_pred = predict(clinic.step,pentaTest)
  preds <- raw_pred*train.sd[20]+train.mean[20]
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  ## fill in the blank list
  gbm_cfs <- summary(clinic.step)
  sing.mod <- data.frame(matrix(ncol = 12, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  
  print(i)
}

coefs <- coefs[-1,]

coef_final <- data.frame("fertility"=c(mean(coefs$fertility),std_mean(coefs$fertility)),
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)),
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)),
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)),
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)),
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                         "population_density"=c(mean(coefs$population_density),std_mean(coefs$population_density)),  
                         "radio"=c(mean(coefs$radio),std_mean(coefs$radio)),  
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "television"=c(mean(coefs$television), std_mean(coefs$television)),
                         "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),
                         "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

View(t(coef_final))




## GAM ----

### feature pruning ----

gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + s(night_lights, k=5) + s(Population,k=5) +
                         s(population_density,k=5) +s(radio, k=5) + s(electricity, k=5)  + s(antenatal_care, k=5) + s(mothers_age, k=5) )

clinic_gam_model <- gam(gam.form, data = pentaTrain, method = "REML") 

clinic_gam_preds <- predict(clinic_gam_model)
R2(pentaTrain[,20],clinic_gam_preds)
adj.r2(R2(pentaTrain[,20],clinic_gam_preds),96,7)

clinic_gam_summary <- summary(clinic_gam_model)
clinic_gam_cfs <- -log10(as.data.frame(clinic_gam_summary$s.table)['p-value'])
xtable(data.frame(clinic_gam_cfs))


### SE ----
set.seed(0)

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, "night_lights"=NA,"Population"=NA,
                    "child_population"=NA, "population_density"=NA, "radio"=NA, "electricity"=NA,
                    "television"=NA,"mobile_phone"=NA, "mothers_age"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

gam.form <- as.formula(TotalClinicsCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + s(night_lights, k=5) + s(Population,k=5) +
                         s(child_population,k=5) +s(population_density,k=5) +s(radio, k=5) + s(electricity, k=5)+s(television, k=5)+s(mobile_phone,k=5) + s(mothers_age, k=5))

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
  names(clinic_gam_cfs) <- c("fertility","elevation","poverty","night_lights","Population","child_population","population_density", 
                             "radio",  "electricity", "television","mobile_phone","mothers_age")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  
  print(i)
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame("fertility"=c(mean(coef_clean$fertility),std_mean(coef_clean$fertility)),
                         "elevation"=c(mean(coef_clean$elevation), std_mean(coef_clean$elevation)),
                         "poverty"=c(mean(coef_clean$poverty), std_mean(coef_clean$poverty)),
                         "night_lights"=c(mean(coef_clean$night_lights), std_mean(coef_clean$night_lights)),
                         "Population"=c(mean(coef_clean$Population), std_mean(coef_clean$Population)),
                         "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                         "population_density"=c(mean(coef_clean$population_density),std_mean(coef_clean$population_density)),  
                         "radio"=c(mean(coef_clean$radio),std_mean(coef_clean$radio)),  
                         "electricity"=c(mean(coef_clean$electricity), std_mean(coef_clean$electricity)),
                         "television"=c(mean(coef_clean$television), std_mean(coef_clean$television)),
                         "mobile_phone"=c(mean(coef_clean$mobile_phone), std_mean(coef_clean$mobile_phone)),
                         "mothers_age"=c(mean(coef_clean$mothers_age), std_mean(coef_clean$mothers_age)))


mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
View(t(coef_final))





## ridge Model ----

### feature pruning ----

y <- pentaTrain$OutreachProportion
x <- data.matrix(pentaTrain[, c(2,3,8:10,13,15,16)])

ridge_model <- cv.glmnet(x, y, alpha = 0, standardize = F)

best_lambda <- ridge_model$lambda.min

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda, standardize = F)

clinic_ridge_preds <- predict(best_model, newx=data.matrix(pentaTrain[,c(2,3,8:10,13,15,16)]))
R2(pentaTrain[,20],clinic_ridge_preds)
adj.r2(R2(pentaTrain[,20],clinic_ridge_preds),96,11)

ridge_outcome <- coef(best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))

### SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000),  "elevation"=rep(0, 1000),  "poverty"=rep(0, 1000), 
                    "child_population"=rep(0, 1000), "population_density"=rep(0, 1000), "radio"=rep(0, 1000),  
                    "mobile_phone"=rep(0, 1000),"antenatal_care"=rep(0, 1000),"mothers_age"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  y <- sample_d$OutreachProportion
  x <- data.matrix(sample_d[, c(2,3,8:10,13,15,16)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(2,3,8:10,13,15,16)]))
  preds <- raw_preds*train.sd[20]+train.mean[20]
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}


coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)),
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)),
                         "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
View(t(coef_final))



#### SE With lambda fixed ----

lmod <- lm(OutreachProportion ~., data=pentaTrain[,c(1,3,6:9,11,12,14,19)])
vif(lmod)

y <- pentaTrain[, "OutreachProportion"]
X <- data.matrix(pentaTrain[,c(1,3,6:9,11,12,14)])

lambda <- c(0, 0.1, 0.2, 0.4, 0.5, 0.8, 1,1.1,1.2,1.3,1.4,1.5,1.6,2)  ## 1.3
lridge <- ridge(y,X, lambda=lambda)
coef(lridge)

vridge <- vif(lridge)
vridge


set.seed(0)
coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000),  "poverty"=rep(0, 1000), 
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000), "radio"=rep(0, 1000), "television"=rep(0, 1000),
                    "mobile_phone"=rep(0, 1000), "antenatal_care"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000),"lambda" = rep(0,1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$OutreachProportion
  x <- data.matrix(sample_d[, c(1,3,6:9,11,12,14)])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 1.3)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1,3,6:9,11,12,14)]))
  rmse <- rmse(pentaTest[,19],preds)
  r2 <- R2(pentaTest[,19],preds)
  mae <- MAE(pentaTest[,19],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)),
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "television"=c(mean(coefs$television), std_mean(coefs$television)),
                         "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

plot(coefs$population_density)


### log transformation ----

#### y ~ log(x) ----

log.train.raw <- pentaTrain.raw[, c(1:4,7:13,16)]
log.train.raw <- apply(log.train.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.train.raw)) {
  if (sum(is.na(log.train.raw[,i])) > 0) {
    log.train.raw[is.na(log.train.raw[,i]),i] <- mean(log.train.raw[,i],na.rm=TRUE)
  }
}
# add outcome col
log.train.raw <- cbind(log.train.raw, pentaTrain.raw[,20])
colnames(log.train.raw)[13] <- "TotalClinicsCoverage"

# log transformation on test set
log.test.raw <- pentaTest.raw[, c(1:4,7:13,16)]
log.test.raw <- apply(log.test.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.test.raw)) {
  if (sum(is.na(log.test.raw[,i])) > 0) {
    log.test.raw[is.na(log.test.raw[,i]),i] <- mean(log.test.raw[,i],na.rm=TRUE)
  }
}
# add outcome col
log.test.raw <- cbind(log.test.raw, pentaTest.raw[,20])
colnames(log.test.raw)[13] <- "TotalClinicsCoverage"


##### without pruning ----
# model fitting
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000),  "poverty"=rep(0, 1000), 
                    "night_lights"=rep(0, 1000),"Population"=rep(0, 1000),"child_population"=rep(0, 1000), "population_density"=rep(0, 1000), 
                    "radio"=rep(0, 1000), "electricity"=rep(0, 1000),"television"=rep(0, 1000),"mobile_phone"=rep(0, 1000),"mothers_age"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = log.train.raw[sample(1:nrow(log.train.raw), nrow(log.train.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(log.test.raw,train.mean,train.sd)
  
  y <- sample_d$TotalClinicsCoverage
  x <- data.matrix(sample_d[, -13])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-13]))
  preds <- raw_preds*train.sd[13]+train.mean[13]
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}


coef_final_log1 <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "fertility"= c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)),
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)), 
                         "television"=c(mean(coefs$television), std_mean(coefs$television)), 
                         "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),
                         "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))

metrics.log1 <- data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
metrics.log1
View(t(coef_final_log1))


##### pruning ----
y <- pentaTrain$TotalClinicsCoverage  #scaled

x <- pentaTrain.raw[, c(1,2,4,7:13,16)]
x <- apply(x, 2, log) %>%
  as.data.frame()
for (i in 1:ncol(x)) {
  if (sum(is.na(x[,i])) > 0) {
    x[is.na(x[,i]),i] <- mean(x[,i],na.rm=TRUE)
  }
}
x <- scale(x)

ridge_model <- cv.glmnet(x, y, alpha = 0, standardize = F)

best_lambda <- ridge_model$lambda.min

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda, standardize = F)

clinic_ridge_preds <- predict(best_model, newx=x)
R2(pentaTrain[,20],clinic_ridge_preds)
adj.r2(R2(pentaTrain[,20],clinic_ridge_preds),96,11)

ridge_outcome <- coef(best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))



#### log(y) ~ log(x) ----
log.train.raw <- pentaTrain.raw[, c(1:4,7:13,16,20)]
log.train.raw <- apply(log.train.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.train.raw)) {
  if (sum(is.na(log.train.raw[,i])) > 0) {
    log.train.raw[is.na(log.train.raw[,i]),i] <- mean(log.train.raw[,i],na.rm=TRUE)
  }
}

# log transformation on test set
log.test.raw <- pentaTest.raw[, c(1:4,7:13,16, 20)]
log.test.raw <- apply(log.test.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.test.raw)) {
  if (sum(is.na(log.test.raw[,i])) > 0) {
    log.test.raw[is.na(log.test.raw[,i]),i] <- mean(log.test.raw[,i],na.rm=TRUE)
  }
}

# model fitting
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000),  "poverty"=rep(0, 1000), 
                    "night_lights"=rep(0, 1000),"Population"=rep(0, 1000),"child_population"=rep(0, 1000), "population_density"=rep(0, 1000), 
                    "radio"=rep(0, 1000), "electricity"=rep(0, 1000),"television"=rep(0, 1000),"mobile_phone"=rep(0, 1000),"mothers_age"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = log.train.raw[sample(1:nrow(log.train.raw), nrow(log.train.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(log.test.raw,train.mean,train.sd)
  
  y <- sample_d$TotalClinicsCoverage
  x <- data.matrix(sample_d[, -13])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-13]))
  preds <- raw_preds*train.sd[13]+train.mean[13]
  rmse <- rmse(log.test.raw[,13],preds)
  r2 <- R2(log.test.raw[,13],preds)
  mae <- MAE(log.test.raw[,13],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coef_final_log2 <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                              "fertility"= c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                              "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                              "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                              "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)), 
                              "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                              "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                              "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)),
                              "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                              "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)), 
                              "television"=c(mean(coefs$television), std_mean(coefs$television)), 
                              "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),
                              "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))

metrics.log2 <- data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
metrics.log2
View(t(coef_final_log2))


## poverty lmod ----

### excluding night_light
df <- tehsils[,c(4:6,8:18,20,22,25,30,26)] %>%
  scale()%>%
  as.data.frame()
df <- df[complete.cases(df),]
lmod <- lm(OutreachProportion ~., df)
summary(lmod)

### including night_light
df <- tehsils[,c(4:18,20,22,25,30,26)] %>%
  scale()%>%
  as.data.frame()
df <- df[complete.cases(df),]
lmod <- lm(OutreachProportion ~., df)
summary(lmod)



# FOR UC ----

ucs <- read.csv("results/uc_complete_buffer12.csv")
ucs <- ucs[, c(6:12,15)] %>%  # 7 features + last col outcome
  na.omit() 

set.seed(1)
data_split <- sample.split(ucs, SplitRatio = 0.8)
pentaTrain.raw <- subset(ucs, data_split == TRUE) 
train.sd <- apply(pentaTrain.raw, 2, sd)
train.mean <- apply(pentaTrain.raw, 2, mean)

pentaTrain <- scale(pentaTrain.raw) %>%
  as.data.frame()

pentaTest.raw <-subset(ucs, data_split == FALSE) 

pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)

## Feature Selection ----

### RFE Feature Selection ----

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:7], pentaTrain[,8],sizes=c(1:7), rfeControl=rfcontrol)

# summarize the results

print(results)
predictors(results)

### Boruta Selection ----

boruta_output <- Boruta(OutreachProportion ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)



## ridge ----

### VIF for lambda selection ----

lmod <- lm(OutreachProportion ~., data=pentaTrain)
vif(lmod)

y <- pentaTrain[, "OutreachProportion"]
X <- data.matrix(pentaTrain[, c(1:7)])

lambda <- c(0, 1,5,10,20,22,23,30,40,50)
lridge <- ridge(y,X, lambda=lambda)
coef(lridge)

vridge <- vif(lridge)
vridge

# plot VIFs
pch <- c(15:18, 7, 9)
clr <- c("black", rainbow(5, start=.6, end=.1))

matplot(rownames(vridge), vridge, type='b', 
        xlab='Ridge constant (k)', ylab="Variance Inflation", 
        xlim=c(0, 50), 
        col=clr, pch=pch, cex=1.2)
text(0.0, vridge[1,], colnames(vridge), pos=4)


### SE With lambda fixed ----

set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  y <- sample_d$OutreachProportion
  x <- data.matrix(sample_d[, -8])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda =23)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-8]))
  preds <- raw_preds*train.sd[8]+train.mean[8]
  rmse <- rmse(pentaTest.raw[,8],preds)
  r2 <- R2(pentaTest.raw[,8],preds)
  mae <- MAE(pentaTest.raw[,8],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

View(t(coef_final))



# adjusted r-squared ----

adj_r2 <- function(r.squared, n, p) {
  return (1 - (1 - r.squared) * ((n - 1)/(n-p-1)))
}

adj_r2()


## poverty lmod ----

df <- ucs %>%
  scale()%>%
  as.data.frame()
df <- df[complete.cases(df),]
lmod <- lm(OutreachProportion ~., df)
summary(lmod)


## GAM ----

gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5) + s(elevation, k=5)  + s(poverty, k=5) +  s(distance_to_cities, k=5) +
                         s(Population, k=5) + s(child_population, k=5)  + s(population_density,k=5) )


set.seed(0)

coefs <- data.frame("fertility"=NA,"elevation"=NA, "poverty"=NA, "distance_to_cities"=NA, "Population"=NA, "child_population"=NA, "population_density"=NA)

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
  preds <- raw_pred*train.sd[8]+train.mean[8]
  rmse <- rmse(pentaTest.raw[,8],preds)
  r2 <- R2(pentaTest.raw[,8],preds)
  mae <- MAE(pentaTest.raw[8],preds)
  
  
  ## fill in the blank list
  
  clinic_gam_summary <- summary(gam.mod$finalModel)
  clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  clinic_gam_cfs  <- as.data.frame(t(clinic_gam_cfs))
  names(clinic_gam_cfs) <- c("fertility","elevation" ,"poverty","distance_to_cities", "Population","child_population","population_density")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  
  print(i)
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame( "fertility"=c(mean(coef_clean$fertility),std_mean(coef_clean$fertility)), 
                          "elevation"=c(mean(coef_clean$elevation),std_mean(coef_clean$elevation)), 
                          "poverty"=c(mean(coef_clean$poverty), std_mean(coef_clean$poverty)),
                          "distance_to_cities"=c(mean(coef_clean$distance_to_cities),std_mean(coef_clean$distance_to_cities)), 
                          "Population"=c(mean(coef_clean$Population), std_mean(coef_clean$Population)), 
                          "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                          "population_density"=c(mean(coef_clean$population_density), std_mean(coef_clean$population_density)))


mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
