# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
#  Pentavalent Vacc Coverage via outreach program
# 2.  Develop Models Using Such Covariates in Order to Predict this outreach 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  

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

# Tehsil ----

### Split Data


### Take the existing Tehsil level data with covariates and Vaccination outreachs and parse out the 
### covariates from the Y (Outreach Vaccination Coverage)

tehsils <- read.csv("results/tehsils_complete_buffer12_9.27.csv")
tehsils.outreach <- tehsils[,c(3:17,19,21:23,27)]

tehsils.outreach <- tehsils.outreach[complete.cases(tehsils.outreach),]

### Split Tehsil data into train and test set

set.seed(43)
data_split = sample.split(tehsils.outreach, SplitRatio = 0.8)

pentaTrain.raw <- subset(tehsils.outreach, data_split == TRUE) 
train.sd <- apply(pentaTrain.raw, 2, sd)
train.mean <- apply(pentaTrain.raw, 2, mean)

pentaTrain <- scale(pentaTrain.raw) %>%
  as.data.frame()

pentaTest.raw <-subset(tehsils.outreach, data_split == FALSE) 



## Feature Selection ----

### RFE Feature Selection ----
### Use Recursive Feature Elimination for Selection of Signficant Features

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:19], pentaTrain[,20],sizes=c(1:19), rfeControl=rfcontrol)

# summarize the results

print(results)
predictors(results)


### Boruta ----
# Use Boruta Selection as another metric to find significant feats

set.seed(10)

boruta_output <- Boruta(TotalOutreachCoverage ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

## Plot signficance of covariates in predicting Y and then determine which are listed as confirmed, 
## tentative or rejected.   Tentative and Confirmed Covariates will be ultimately considered significant

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)

### Those covariates that were determined 
### as confirmed or tentatively significant by the Boruta Models along with those that 
### were deemed as signfiicant by the RFE featire selection should be those included in modeling
### c(4,6:8,11,15,16)


## GBM ----

### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation

outreach.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(6:8,11,15,16),
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10
)

gbm_pred = predict(outreach.step,pentaTrain)
gbm_rsquared <- R2(pentaTrain[,20],gbm_pred)
adj.r2(gbm_rsquared,96,6)

# Get the relative influences provided by GBM to see which features are being most utilized by the model

gbm_cfs <- summary(outreach.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))


#### SE ----
set.seed(10)

coefs <- data.frame("distance_to_cities"=NA, "Population"=NA, 
                    "child_population"=NA,"electricity"=NA,"antenatal_care"=NA, "mothers_age"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  outreach.step <- gbm.step(
    data=sample_d, 
    gbm.x =  c(6:8,11,15,16),   # selected features 
    gbm.y = 20,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.005,
    bag.fraction = 0.5,
    cv_folds = 10,
    plot.main = F,
    verbose = F
  )
  
  raw_pred = predict(outreach.step,pentaTest)
  preds <- raw_pred*train.sd[20]+train.mean[20]
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  ## fill in the blank list
  
  gbm_cfs <- summary(outreach.step)
  sing.mod <- data.frame(matrix(ncol = 6, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  
  print(i)
}

coefs <- coefs[-1,]

coef_final <- data.frame("distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)),
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)),
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)),
                         "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))
View(t(coef_final))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




## GAM ----

gam.form <- as.formula(TotalOutreachCoverage ~ s(night_lights, k=5) + 
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + s(electricity, k=5)+ 
                         s(antenatal_care, k=5) + s(mothers_age, k=5))

outreach_gam_model <- gam(gam.form, data = pentaTrain, method = "REML") 

outreach_gam_preds <- predict(outreach_gam_model,pentaTrain)
outreach_gam_R2 <- R2(pentaTrain[,20],outreach_gam_preds)
adj.r2(outreach_gam_R2,96,7)

outreach_gam_summary <- summary(outreach_gam_model)
outreach_gam_cfs <- -log10(as.data.frame(outreach_gam_summary$s.table)['p-value'])
xtable(data.frame(outreach_gam_cfs))

#### SE ----
set.seed(0)

coefs <- data.frame("night_lights"=NA, "distance_to_cities"=NA, "Population"=NA,
                    "child_population"=NA,"electricity"=NA, "antenatal_care"=NA, "mothers_age"=NA)

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
  names(clinic_gam_cfs) <- c( "night_lights", "distance_to_cities", "Population",
                             "child_population","electricity", "antenatal_care", "mothers_age")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame("night_lights"=c(mean(coef_clean$night_lights), std_mean(coef_clean$night_lights)),
                          "distance_to_cities"=c(mean(coef_clean$distance_to_cities), std_mean(coef_clean$distance_to_cities)),
                          "Population"=c(mean(coef_clean$Population), std_mean(coef_clean$Population)), 
                          "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                         "electricity"=c(mean(coef_clean$electricity), std_mean(coef_clean$electricity)),
                          "antenatal_care"=c(mean(coef_clean$antenatal_care),std_mean(coef_clean$antenatal_care)),  
                          "mothers_age"=c(mean(coef_clean$mothers_age),std_mean(coef_clean$mothers_age)))
                         
View(t(coef_final))                         
mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))





## Ridge ----

y <- pentaTrain$TotalOutreachCoverage
x <- data.matrix(pentaTrain[,c(4,6:8,11,15)])

ridge_model <- cv.glmnet(x, y, alpha = 0, standardize = F)

best_lambda <- ridge_model$lambda.min

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda, standardize = F)

outreach_ridge_preds <- predict(ridge_best_model, newx=data.matrix(pentaTrain[,c(4,6:8,11,15)]))
outreach_ridge_R2 <- R2(pentaTrain[,20],outreach_ridge_preds)
adj.r2(outreach_ridge_R2,96,6)

ridge_outcome <- coef(ridge_best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))



### SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "night_lights"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), 
                    "electricity"=rep(0, 1000),"antenatal_care"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, c(4,6:8,11,15)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(4,6:8,11,15)]))
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
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)))
View(t(coef_final))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))





#### SE With lambda fixed ----

lmod <- lm(TotalOutreachCoverage ~., data=pentaTrain[,c(4,6:8,10,11,15,16,20)])
vif(lmod)

y <- pentaTrain[, "TotalOutreachCoverage"]
X <- data.matrix(pentaTrain[, c(4,6:8,10,11,15,16)])

lambda <- c(0, 0.1, 0.2, 0.4, 0.5, 0.8, 1,1.1,1.2,1.3,1.4,1.5,1.6,2)  ## 1.3
lridge <- ridge(y,X, lambda=lambda)
coef(lridge)

vridge <- vif(lridge)
vridge


set.seed(0)
coefs <- data.frame("Intercept"= rep(0, 1000), "night_lights"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "radio"=rep(0, 1000),"electricity"=rep(0, 1000),
                    "antenatal_care"=rep(0, 1000), "mothers_age"=rep(0, 1000))
mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000),"lambda" = rep(0,1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, c(4,6:8,10,11,15,16)])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 1.3)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(4,6:8,10,11,15,16)]))
  rmse <- rmse(pentaTest[,20],preds)
  r2 <- R2(pentaTest[,20],preds)
  mae <- MAE(pentaTest[,20],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)),
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)),
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)),
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)),
                         "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

plot(coefs$population_density)





#### y ~ log(x) ----
log.train.raw <- pentaTrain.raw[, c(1,2,4,6:8,15,16)]
log.train.raw <- apply(log.train.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.train.raw)) {
  if (sum(is.na(log.train.raw[,i])) > 0) {
    log.train.raw[is.na(log.train.raw[,i]),i] <- mean(log.train.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.train.raw[,i])) > 0) {
    log.train.raw[is.infinite(log.train.raw[,i]),i] <- mean(log.train.raw[!is.infinite(log.train.raw[,i]),i],na.rm=TRUE)
  }
}
# add outcome col
log.train.raw <- cbind(log.train.raw, pentaTrain.raw[,20])
colnames(log.train.raw)[9] <- "TotalOutreachCoverage"

# log transformation on test set
log.test.raw <- pentaTest.raw[, c(1,2,4,6:8,15,16)]
log.test.raw <- apply(log.test.raw, 2, log) %>%
  as.data.frame()
# na and inf produced, fill with mean
for (i in 1:ncol(log.test.raw)) {
  if (sum(is.na(log.test.raw[,i])) > 0) {
    log.test.raw[is.na(log.test.raw[,i]),i] <- mean(log.test.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.test.raw[,i])) > 0) {
    log.test.raw[is.infinite(log.test.raw[,i]),i] <- mean(log.test.raw[!is.infinite(log.test.raw[,i]),i],na.rm=TRUE)
  }
}
# add outcome col
log.test.raw <- cbind(log.test.raw, pentaTest.raw[,20])
colnames(log.test.raw)[9] <- "TotalOutreachCoverage"

# model fitting
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), 
                    "night_lights"=rep(0, 1000),"distance_to_cities"=rep(0, 1000),"Population"=rep(0, 1000),"child_population"=rep(0, 1000),
                    "antenatal_care"=rep(0, 1000),"mothers_age"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = log.train.raw[sample(1:nrow(log.train.raw), nrow(log.train.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(log.test.raw,train.mean,train.sd)
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, -9])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-9]))
  preds <- raw_preds*train.sd[9]+train.mean[9]
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
                              "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)), 
                              "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                              "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                              "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                              "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)),
                              "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))

metrics.log1 <- data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
metrics.log1
View(t(coef_final_log1))

#### log(y) ~ log(x) ----
log.train.raw <- pentaTrain.raw[, c(1,2,4,6:8,15,16,20)]
log.train.raw <- apply(log.train.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.train.raw)) {
  if (sum(is.na(log.train.raw[,i])) > 0) {
    log.train.raw[is.na(log.train.raw[,i]),i] <- mean(log.train.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.train.raw[,i])) > 0) {
    log.train.raw[is.infinite(log.train.raw[,i]),i] <- mean(log.train.raw[!is.infinite(log.train.raw[,i]),i],na.rm=TRUE)
  }
}

# log transformation on test set
log.test.raw <- pentaTest.raw[, c(1,2,4,6:8,15,16,20)]
log.test.raw <- apply(log.test.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.test.raw)) {
  if (sum(is.na(log.test.raw[,i])) > 0) {
    log.test.raw[is.na(log.test.raw[,i]),i] <- mean(log.test.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.test.raw[,i])) > 0) {
    log.test.raw[is.infinite(log.test.raw[,i]),i] <- mean(log.test.raw[!is.infinite(log.test.raw[,i]),i],na.rm=TRUE)
  }
}

# model fitting
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), 
                    "night_lights"=rep(0, 1000),"distance_to_cities"=rep(0, 1000),"Population"=rep(0, 1000),"child_population"=rep(0, 1000),
                    "antenatal_care"=rep(0, 1000),"mothers_age"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = log.train.raw[sample(1:nrow(log.train.raw), nrow(log.train.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(log.test.raw,train.mean,train.sd)
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, -9])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-9]))
  preds <- raw_preds*train.sd[9]+train.mean[9]
  rmse <- rmse(log.test.raw[,9],preds)
  r2 <- R2(log.test.raw[,9],preds)
  mae <- MAE(log.test.raw[,9],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coef_final_log2 <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                              "fertility"= c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                              "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                              "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)), 
                              "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                              "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                              "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                              "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)),
                              "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))

metrics.log2 <- data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
metrics.log2
View(t(coef_final_log2))


# test edu_mode ----

tehsil.test <- read.csv("results/tehsils_complete_7.19.csv")
df <- merge(tehsil.test,df3,by=x, all.x=T)
View(df)
tehsil.outreach <- df[,c(26,3:21,24,29)]

tehsil.outreach <- as.data.frame(df[,c(26,3:21,24,29)]) %>% scale() %>% as.data.frame()
set.seed(43)
data_split = sample.split(tehsils.outreach, SplitRatio = 0.8)

tehsils.outreach <- tehsil.outreach
set.seed(43)
data_split = sample.split(tehsils.outreach, SplitRatio = 0.8)
pentaTrain <- subset(tehsils.outreach, data_split == TRUE)
pentaTest <-subset(tehsils.outreach, data_split == FALSE)
outreach.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(2:22),
  gbm.y = 1,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)



# FOR UC ----

ucs <- read.csv("results/uc_complete_buffer45.csv")
ucs <- ucs[, c(6:12, 16)] %>%  # 7 features + last col outcome
  na.omit()

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

set.seed(1)

boruta_output <- Boruta(TotalOutreachCoverage ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)


## chosen features
## c(1,2,5:7)



## ridge ----

### VIF for lambda selection ----

lmod <- lm(TotalOutreachCoverage ~., data=pentaTrain)
vif(lmod)

y <- pentaTrain[, "TotalOutreachCoverage"]
X <- data.matrix(pentaTrain[, c(1,2,5:7)])

lambda <- c(0, 0.005, 0.01, 0.02, 0.04, 0.08, 1,5,10,20,22,23,24,25,30,40,50)
lridge <- ridge(y,X, lambda=lambda)
coef(lridge)

vridge <- vif(lridge)
vridge

# lambda = 23

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

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), 
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, c(2,3,5:7)])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 23, standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(2,3,5:7)]))
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
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

View(t(coef_final))




## GBM ----
set.seed(0)

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA,  "distance_to_cities"=NA, "Population"=NA, 
                    "child_population"=NA,"population_density"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x = c(1:7),   # selected features 
    gbm.y = 8,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.005,
    bag.fraction = 0.5,
    cv_folds = 10,
    plot.main = F,
    verbose = F
  )
  
  gbm_pred = predict(clinic.step,pentaTest)
  rmse <- rmse(pentaTest[,8],gbm_pred)
  r2 <- R2(pentaTest[,8],gbm_pred)
  mae <- mae(pentaTest[,8],gbm_pred)
  
  
  ## fill in the blank list
  
  gbm_cfs <- summary(clinic.step)
  sing.mod <- data.frame(matrix(ncol = 7, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  
  print(i)
}

coefs <- coefs[-1,]

coef_final <- data.frame("fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




