# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
# in Clinic Pentavalent Vacc Coverage
# 2.  Develop Models Using Such Covariates in Order to Predict this In Clinic 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  

source(file='PreRunNew.r')
library(genridge)
library(Boruta)
library(mgcv)
library(glmnet)
std_mean <- function(x) sd(x)/sqrt(length(x))


# For Tehsil ----

### Take the existing Tehsil level data with covariates and Vaccination ratios and parse out the 
### covariates from the Y (Clinic Vaccination Coverage)

# tehsils <- read.csv("results/tehsils_complete_8.15.csv")
tehsils.clinic <- tehsils[,c(4:18,20,22,25,30,28)] %>%   # 19 features + last col the outcome
  scale() %>%
  as.data.frame() 

tehsils.clinic <- tehsils.clinic[complete.cases(tehsils.clinic), ]  


### Split Tehsil data into train and test set

set.seed(0)

data_split = sample.split(tehsils.clinic, SplitRatio = 0.8)
pentaTrain <- subset(tehsils.clinic, data_split == TRUE)
pentaTest <-subset(tehsils.clinic, data_split == FALSE)


## Feature selection ---- 

### RFE Feature Selection ---- 
### Use Recursive Feature Elimination for Selection of Signficant Features

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:19], pentaTrain[,20],sizes=c(1:19), rfeControl=rfcontrol)

# summarize the results

print(results)
rfe_sig <- predictors(results)  
rfe_sig


### Boruta ----

## Use Boruta Selection as another metric to find significant feats

set.seed(0)

boruta_output <- Boruta(TotalClinicsCoverage ~ ., data=na.omit(pentaTrain), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # print significant variable rankings

## Plot signficance of covariates in predicting Y and then determine which are listed as confirmed, 
## tentative or rejected.   Tentative and Confirmed Covariates will be ultimately considered significant

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)




## Build Models ----

### Those covariates that were determined as confirmed or tentatively significant by the Boruta Models along with those that 
### were deemed as signfiicant by the RFE featire selection should be those included in modeling
### c(1:4,6:13,16)


### GBM ----
### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation


clinic.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:4,6:8,10:13,16),   # selected features 
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(clinic.step,pentaTest)
gbm_rmse <- rmse(pentaTest[,20],gbm_pred)
gbm_rsquared <- R2(pentaTest[,20],gbm_pred)
gbm_mae <- mae(pentaTest[,20],gbm_pred)

### What features did the GBM model identify as significant in predicting our Y?

gbm_cfs <- summary(clinic.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))

#### SE ----
set.seed(0)

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA,"night_lights"=NA, "distance_to_cities"=NA, "Population"=NA,
                    "child_population"=NA,  "radio"=NA, "electricity"=NA,"television"=NA,
                    "mobile_phone"=NA,"mothers_age" =NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x =  c(1:4,6:8,10:13,16),   # selected features 
    gbm.y = 20,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.005,
    bag.fraction = 0.5,
    cv_folds = 10,
  )
  
  gbm_pred = predict(clinic.step,pentaTest)
  rmse <- rmse(pentaTest[,20],gbm_pred)
  r2 <- R2(pentaTest[,20],gbm_pred)
  mae <- mae(pentaTest[,20],gbm_pred)
  
  
  ## fill in the blank list
  
  gbm_cfs <- summary(clinic.step)
  sing.mod <- data.frame(matrix(ncol = 12, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_final <- data.frame("radio"=c(mean(coefs$radio),std_mean(coefs$radio)),  
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                         "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)),
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)),
                         "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)),
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)),
                          "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "television"=c(mean(coefs$television), std_mean(coefs$television)),
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))





### GAM ----


### Evaluate Performances of GAM Model using RMSE,R2,MAE


gam.form <- as.formula(TotalClinicsCoverage ~ s(elevation, k=5) + s(poverty, k=5) +  s(night_lights, k=5) + s(distance_to_cities, k=5) +
                         s(Population, k=5) + s(child_population, k=5) + s(population_density, k=5) + 
                         s(radio, k=5)+ s(television, k=5)  + s(mothers_age, k=5))

gam.mod <- gam(gam.form, data = pentaTrain, method = "REML")  

gam_preds <- predict(gam.mod, pentaTest)

clinic_gam_RMSE <- rmse(pentaTest[,20],gam_preds)
clinic_gam_R2 <- R2(pentaTest[,20],gam_preds)
clinic_gam_MAE <- MAE(pentaTest[,20],gam_preds)

clinic_gam_summary <- summary(gam.mod$finalModel)
clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
xtable(data.frame(clinic_gam_cfs))

#### SE ----
set.seed(10)

coefs <- data.frame("elevation"=NA, "poverty"=NA, "night_lights"=NA, "distance_to_cities"=NA, "Population"=NA,
                    "child_population"=NA, "population_density"=NA,  "radio"=NA, 
                    "television"=NA,"mothers_age" =NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  gam.mod <- gam(gam.form, data = sample_d, method = "REML") 
  
  gam_preds <- predict(gam.mod, pentaTest)
  rmse <- rmse(pentaTest[,20],gam_preds)
  r2 <- R2(pentaTest[,20],gam_preds)
  mae <- mae(pentaTest[,20],gam_preds)
  
  
  ## fill in the blank list
  
  clinic_gam_summary <- summary(gam.mod$finalModel)
  clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  clinic_gam_cfs  <- as.data.frame(t(clinic_gam_cfs))
  names(clinic_gam_cfs) <- c("elevation","poverty","night_lights","distance_to_cities","Population","child_population","population_density","radio", 
                             "television", "mothers_age")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame("radio"=c(mean(coef_clean$radio),std_mean(coef_clean$radio)),  
                         "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                         "population_density"=c(mean(coef_clean$population_density), std_mean(coef_clean$population_density)), 
                         "Population"=c(mean(coef_clean$Population), std_mean(coef_clean$Population)), 
                         "distance_to_cities"=c(mean(coef_clean$distance_to_cities), std_mean(coef_clean$distance_to_cities)),
                         "poverty"=c(mean(coef_clean$poverty), std_mean(coef_clean$poverty)),
                         "mothers_age"=c(mean(coef_clean$mothers_age), std_mean(coef_clean$mothers_age)),
                         "elevation"=c(mean(coef_clean$elevation), std_mean(coef_clean$elevation)),
                         "night_lights"=c(mean(coef_clean$night_lights), std_mean(coef_clean$night_lights)),
                         "television"=c(mean(coef_clean$television), std_mean(coef_clean$television)))
mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))





### Ridge ----


y <- pentaTrain$TotalClinicsCoverage
x <- data.matrix(pentaTrain[, c(1,3,4,6:10,12)])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min
best_lambda

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(ridge_best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))


clinic_lasso_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1,3,4,6:10,12)]))
clinic_lasso_RMSE <- rmse(pentaTest[,20],clinic_lasso_preds)
clinic_lasso_R2 <- R2(pentaTest[,20],clinic_lasso_preds)
clinic_lasso_MAE <- MAE(pentaTest[,20],clinic_lasso_preds)

#### SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "poverty"=rep(0, 1000), "night_lights"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000),  "radio"=rep(0, 1000),"television"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000),"lambda" = rep(0,1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalClinicsCoverage
  x <- data.matrix(sample_d[, c(1,3,4,6:10,12)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1,3,4,6:10,12)]))
  rmse <- rmse(pentaTest[,20],preds)
  r2 <- R2(pentaTest[,20],preds)
  mae <- MAE(pentaTest[,20],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
  mod_performance[i,4] <- best_lambda
}

coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)),
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)),
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)), 
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)),
                         "television"=c(mean(coefs$television), std_mean(coefs$television)) )
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))



#### SE With lambda fixed ----

lmod <- lm(TotalClinicsCoverage ~., data=pentaTrain[,c(1,3,4,6:10,12,20)])
vif(lmod)

y <- pentaTrain[, "TotalClinicsCoverage"]
X <- data.matrix(pentaTrain[, c(1,3,4,6:10,12)])

lambda <- c(0, 0.1, 0.2, 0.4, 0.5, 0.8, 1,1.1,1.2,1.3,1.4,1.5,1.6,2)  ## 1.4
lridge <- ridge(y,X, lambda=lambda)
coef(lridge)

vridge <- vif(lridge)
vridge


set.seed(0)
coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "poverty"=rep(0, 1000), "night_lights"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000),  "radio"=rep(0, 1000),"television"=rep(0, 1000))
mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000),"lambda" = rep(0,1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalClinicsCoverage
  x <- data.matrix(sample_d[, c(1,3,4,6:10,12)])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 1.4)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1,3,4,6:10,12)]))
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
                         "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "night_lights"=c(mean(coefs$night_lights), std_mean(coefs$night_lights)),
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)),
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)), 
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)),
                         "television"=c(mean(coefs$television), std_mean(coefs$television)) )
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

plot(coefs$population_density)



# FOR UC ----

# ucs <- read.csv("results/uc_complete_clean.csv")
ucs <- ucs[, c(5:11,14)] %>%  # 7 features + last col outcome
  na.omit() %>%
  scale() %>%
  as.data.frame()

set.seed(1)
data_split <- sample.split(ucs, SplitRatio = 0.8)
pentaTrain <- subset(ucs, data_split == TRUE)
pentaTest <-subset(ucs, data_split == FALSE)



## Feature selection ----

### RFE Feature Selection ----

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:7], pentaTrain[,8],sizes=c(1:7), rfeControl=rfcontrol)

# summarize the results

print(results)
predictors(results)


### Boruta Selection ----

set.seed(1)
boruta_output <- Boruta(TotalClinicsCoverage ~ ., data=na.omit(pentaTrain), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)


## choose lambda for Ridge ----


lmod <- lm(TotalClinicsCoverage ~., data=pentaTrain)
vif(lmod)


## SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalClinicsCoverage
  x <- data.matrix(sample_d[, c(1,3,4)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1,3,4)]))
  rmse <- rmse(pentaTest[,8],preds)
  r2 <- R2(pentaTest[,8],preds)
  mae <- MAE(pentaTest[,8],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}


coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)),
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

