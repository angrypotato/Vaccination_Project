# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
# in Clinic Pentavalent Vacc Coverage
# 2.  Develop Models Using Such Covariates in Order to Predict this In Clinic 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  


source(file='VaccinationStudy/PreRun.r')

### Split Data

set.seed(42)

### Take the existing Tehsil level data with covariates and Vaccination ratios and parse out the 
### covariates from the Y (Clinic Vaccination Coverage)

tehsils <- tehsils[,c(10:12,14:18,22:27,29,32,40)]

### Split Tehsil data into train and test set

data_split = sample.split(tehsils, SplitRatio = 0.8)
pentaTrain <- subset(tehsils, data_split == TRUE)
pentaTest <-subset(tehsils, data_split == FALSE)

### RFE Feature Selection
### Use Recursive Feature Elimination for Selection of Signficant Features

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:16], pentaTrain[,17],sizes=c(1:16), rfeControl=rfcontrol)

# summarize the results

print(results)
predictors(results)

# Use Boruta Selection as another metric to find significant feats

boruta_output <- Boruta(TotalClinicsCoverage ~ ., data=na.omit(sub_out), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # print significant variable rankings

## Plot signficance of covariates in predicting Y and then determine which are listed as confirmed, 
## tentative or rejected.   Tentative and Confirmed Covariates will be ultimately considered significant

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)

### Those covariates that were determined 
### as confirmed or tentatively significant by the Boruta Models along with those that 
### were deemed as signfiicant by the RFE featire selection should be those included in modeling

### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation

ratio.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = 1:7,
  gbm.y = 8,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(ratio.step,pentaTest,1000)
gbm_rmse <- rmse(pentaTest[,8],gbm_pred)
gbm_rsquared <- R2(pentaTest[,8],gbm_pred)
gbm_mae <- mae(pentaTest[,8],gbm_pred)

### What features did the GBM model identify as significant in predicting our Y (Outreach/Clinic Vacc ratio)?

gbm_cfs <- summary(ratio.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))

# Attain Evaluation Metrics for performance of model using covariates selected

# Define Params for Cross Validation and Grid Search - will be used in modeling

control <- trainControl(method="repeatedcv", number=10, repeats=3,search="grid")

### Producing the GAM Model - tune with K-Folds Validation,Grid Search

ratio_gam_model <-train(TotalClinicsCoverage~., data = pentaTrain, method="gam", trControl=control, tuneLength=5)
ratio_gam_preds <- predict(ratio_gam_model,pentaTest)

### Evaluate Performances of GAM Model using RMSE,R2,MAE

ratio_gam_RMSE <- rmse(pentaTest[,13],ratio_gam_preds)
ratio_gam_R2 <- R2(pentaTest[,13],ratio_gam_preds)
ratio_gam_MAE <- MAE(pentaTest[,13],ratio_gam_preds)
ratio_gam_summary <- summary(ratio_gam_model$finalModel)
ratio_gam_cfs <- -log10(as.data.frame(ratio_gam_summary$s.table)['p-value'])
xtable(data.frame(ratio_gam_cfs))

### Producing the Lasso Model

ratio_lasso_model <- train(TotalClinicsCoverage~., data=pentaTrain, method="lasso", trControl=control, tuneLength=5)
ratio_lasso_preds <- predict(ratio_lasso_model,pentaTest)
ratio_lasso_RMSE <- rmse(pentaTest[,17],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,17],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,17],ratio_lasso_preds)

# Try different Coefficients of Lasso Model and Check which coefficients produce best results

coefs <- predict.lars(ratio_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
winnermodelscoeffs <- models[4,] # Find the best model
ratio_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(ratio_lasso_cfs)))

### FOR DISTRICTS

set.seed(42)
data_split = sample.split(districts, SplitRatio = 0.8)
pentaTrain <- subset(districts, data_split == TRUE)
pentaTest <-subset(districts, data_split == FALSE)


### RFE Feature Selection

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:16], pentaTrain[,17],sizes=c(1:16), rfeControl=rfcontrol)

# summarize the results

print(results)
predictors(results)

# Boruta Selection

boruta_output <- Boruta(TotalClinicsCoverage ~ ., data=na.omit(sub_out), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)

### Producing the GBM Model

ratio.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = 1:7,
  gbm.y = 8,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(ratio.step,pentaTest,1000)
gbm_rmse <- rmse(pentaTest[,8],gbm_pred)
gbm_rsquared <- R2(pentaTest[,8],gbm_pred)
gbm_mae <- mae(pentaTest[,8],gbm_pred)

gbm_cfs <- summary(ratio.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))

### Producing the GAM Model

ratio_gam_model <-train(TotalClinicsCoverage~., data = pentaTrain, method="gam", trControl=control, tuneLength=5)
ratio_gam_preds <- predict(ratio_gam_model,pentaTest)
ratio_gam_RMSE <- rmse(pentaTest[,13],ratio_gam_preds)
ratio_gam_R2 <- R2(pentaTest[,13],ratio_gam_preds)
ratio_gam_MAE <- MAE(pentaTest[,13],ratio_gam_preds)
ratio_gam_summary <- summary(ratio_gam_model$finalModel)
ratio_gam_cfs <- -log10(as.data.frame(ratio_gam_summary$s.table)['p-value'])
xtable(data.frame(ratio_gam_cfs))

### Producing the Lasso Model

ratio_lasso_model <- train(TotalClinicsCoverage~., data=pentaTrain, method="lasso", trControl=control, tuneLength=5)
ratio_lasso_preds <- predict(ratio_lasso_model,pentaTest)
ratio_lasso_RMSE <- rmse(pentaTest[,17],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,17],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,17],ratio_lasso_preds)

coefs <- predict.lars(ratio_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
winnermodelscoeffs <- models[4,] # Find the best model
ratio_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(ratio_lasso_cfs)))

