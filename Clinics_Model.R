# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
# in Clinic Pentavalent Vacc Coverage
# 2.  Develop Models Using Such Covariates in Order to Predict this In Clinic 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  

source(file='PreRunNew.r')



# For Tehsil ----

### Take the existing Tehsil level data with covariates and Vaccination ratios and parse out the 
### covariates from the Y (Clinic Vaccination Coverage)

# tehsils <- read.csv("results/tehsils_complete.csv")
tehsils <- tehsils[,c(14:26,28,30,32,34,38,39,44)] %>%   # 19 features + last col the outcome
  scale() %>%
  as.data.frame()

### Split Tehsil data into train and test set

set.seed(1)

data_split = sample.split(tehsils, SplitRatio = 0.8)
pentaTrain <- subset(tehsils, data_split == TRUE)
pentaTest <-subset(tehsils, data_split == FALSE)


## Feature selection ---- 

### RFE Feature Selection ---- 
### Use Recursive Feature Elimination for Selection of Signficant Features

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:19], pentaTrain[,20],sizes=c(1:19), rfeControl=rfcontrol)

# summarize the results

print(results)
rfe_sig <- predictors(results)  


### Boruta ----

## Use Boruta Selection as another metric to find significant feats

library(Boruta)

boruta_output <- Boruta(TotalClinicsCoverage ~ ., data=na.omit(pentaTrain), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # print significant variable rankings

## Plot signficance of covariates in predicting Y and then determine which are listed as confirmed, 
## tentative or rejected.   Tentative and Confirmed Covariates will be ultimately considered significant

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)


## Build Models ----

### Those covariates that were determined 
### as confirmed or tentatively significant by the Boruta Models along with those that 
### were deemed as signfiicant by the RFE featire selection should be those included in modeling


### GBM ----
### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation

ratio.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:4,7:13, 16, 18, 19),   # selected features + poverty
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(ratio.step,pentaTest,1000)
gbm_rmse <- rmse(pentaTest[,20],gbm_pred)
gbm_rsquared <- R2(pentaTest[,20],gbm_pred)
gbm_mae <- mae(pentaTest[,20],gbm_pred)

### What features did the GBM model identify as significant in predicting our Y?

gbm_cfs <- summary(ratio.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))


### GAM ----

# Attain Evaluation Metrics for performance of model using covariates selected

# Define Params for Cross Validation and Grid Search - will be used in modeling

control <- trainControl(method="repeatedcv", 
                        number=10,   # k for k-fold CV
                        repeats=3,   
                        search="grid")   # grid search CV (only tuning method, chose GCV.Cp)

### Producing the GAM Model - tune with K-Folds Validation,Grid Search

ratio_gam_model <-train(TotalClinicsCoverage ~ .,
                        data = as.data.frame(pentaTrain[,c( 7:13,  19, 20)]),   
                        method="gam", trControl=control, 
                        crtuneLength=5)   # try this # dif values of tuning parameters 
ratio_gam_preds <- predict(ratio_gam_model,pentaTest)

### Evaluate Performances of GAM Model using RMSE,R2,MAE

ratio_gam_RMSE <- rmse(pentaTest[,20],ratio_gam_preds)
ratio_gam_R2 <- R2(pentaTest[,20],ratio_gam_preds)
ratio_gam_MAE <- MAE(pentaTest[,20],ratio_gam_preds)
ratio_gam_summary <- summary(ratio_gam_model$finalModel)
ratio_gam_cfs <- -log10(as.data.frame(ratio_gam_summary$s.table)['p-value'])
xtable(data.frame(ratio_gam_cfs))




#### other methods ----
library(mgcv)

gam.form <- as.formula(TotalClinicsCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(night_lights, k=5) + 
                         s(Population, k=5) + s(child_p
                                                opulation, k=5) + s(population_density, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(television, k=5) + s(mobile_phone, k=5) + s(mothers_age, k=5) +
                         s(urban_to_rural, k=5) + s(distance_to_cities, k=5) +
                         s(poverty, k = 5))

gam.mod <- gam(gam.form, data = pentaTrain, method = "GCV.Cp")  

gam_preds <- predict(gam.mod, pentaTest, se.fit=T)

ratio_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
xtable(data.frame(ratio_gam_cfs))
### works


# check k: gam.check()

# Error: Model has more coefficients than data 

## check wiggliness
df_chosen <- pentaTrain[,c(1,2,4,7:13, 16, 18, 19, 20)]
par(mfrow=c(5,3))
for (n in 1:(length(df_chosen)-1)) {
  plot(df_chosen[,n], df_chosen[,14])
  curve_values <- loess(df_chosen[,14] ~ df_chosen[,n])
  lines(predict(curve_values), x = df_chosen[,n], col = "red",lwd = 1)
}
par(mfrow=c(1,1))


library(gamreg)
gam.mod <- cv.gam(data.matrix(pentaTrain[,c( 7:13,  19, 20)]), data.matrix(pentaTrain[,20]))




### LASSO ----

### Producing the Lasso Model

ratio_lasso_model <- train(TotalClinicsCoverage~., 
                           data=pentaTrain[,c(1:4,7:13, 16, 18, 19,20)], method="lasso", trControl=control, tuneLength=5)
ratio_lasso_preds <- predict(ratio_lasso_model, newdata=pentaTest)
ratio_lasso_RMSE <- rmse(pentaTest[,20],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,20],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,20],ratio_lasso_preds)

# Try different Coefficients of Lasso Model and Check which coefficients produce best results

coefs <- predict.lars(ratio_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
# ratio_lasso_model$finalModel$Cp 
winnermodelscoeffs <- models[16,] # the best model (smallest Cp)
ratio_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(ratio_lasso_cfs)))




# FOR UC ----

# ucs <- read.csv("results/ucs_complete.csv")
ucs <- ucs[, c(25:31,36)] %>%  # 7 features + last col outcome
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

boruta_output <- Boruta(TotalClinicsCoverage ~ ., data=na.omit(pentaTrain), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)


## Lasso Model ----


ratio_lasso_model <- train(TotalClinicsCoverage~., data=pentaTrain, method="lasso", trControl=control, tuneLength=5)
ratio_lasso_preds <- predict(ratio_lasso_model,pentaTest)
ratio_lasso_RMSE <- rmse(pentaTest[,8],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,8],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,8],ratio_lasso_preds)

coefs <- predict.lars(ratio_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
# ratio_lasso_model$finalModel$Cp
winnermodelscoeffs <- models[6,] # Find the best model (smallest Cp)
ratio_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(ratio_lasso_cfs)))




### 
lmod <- lm(TotalClinicsCoverage ~ ., data = tehsils.plot[,-c(1,3)])
summary(lmod)
