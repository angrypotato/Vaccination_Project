# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
#  Pentavalent Vacc Coverage via outreach program
# 2.  Develop Models Using Such Covariates in Order to Predict this outreach 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  

source(file='VaccinationStudy/PreRun.r')


# Tehsil ----

### Split Data

set.seed(1)

### Take the existing Tehsil level data with covariates and Vaccination ratios and parse out the 
### covariates from the Y (Outreach Vaccination Coverage)

# tehsils <- read.csv("results/tehsils_complete_7.19.csv")
tehsils.outreach <-  tehsils[,c(3:5, 7:21,24,26)] %>%   # 19 features + last col the outcome
  scale() %>%
  as.data.frame()

tehsils.outreach <- tehsils.outreach[complete.cases(tehsils.outreach),]

### Split Tehsil data into train and test set

data_split = sample.split(tehsils.outreach, SplitRatio = 0.8)
pentaTrain <- subset(tehsils.outreach, data_split == TRUE)
pentaTest <-subset(tehsils.outreach, data_split == FALSE)


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


## GBM ----

### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation

ratio.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1,2,5:8,16,19),
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

# Get the relative influences provided by GBM to see which features are being most utilized by the model

gbm_cfs <- summary(ratio.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))




## GAM ----

# Attain Evaluation Metrics for performance of model using covariates selected

library(mgcv)

gam.form <- as.formula(TotalOutreachCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + 
                         s(malaria_instance, k=5) + s(Population, k=5) + s(child_population, k=5) + s(population_density, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(television, k=5) + s(mobile_phone, k=5) + 
                         s(antenatal_care, k=5) + s(mothers_age, k=5) + s(school_level, k=5))

ratio_gam_model <- gam(gam.form, data = pentaTrain, method = "GCV.Cp") 
 
ratio_gam_preds <- predict(ratio_gam_model,pentaTest)

### Evaluate Performances of GAM Model using RMSE,R2,MAE

ratio_gam_RMSE <- rmse(pentaTest[,13],ratio_gam_preds)
ratio_gam_R2 <- R2(pentaTest[,13],ratio_gam_preds)
ratio_gam_MAE <- MAE(pentaTest[,13],ratio_gam_preds)
ratio_gam_summary <- summary(ratio_gam_model)
ratio_gam_cfs <- -log10(as.data.frame(ratio_gam_summary$s.table)['p-value'])
xtable(data.frame(ratio_gam_cfs))


## LASSO ----

### Producing the Lasso Model

control <- trainControl(method="repeatedcv", number=10, repeats=3,search="grid")
ratio_lasso_model <- train(TotalOutreachCoverage~., data=pentaTrain[,c(1:3,6:13,15:17,20)], 
                           method="lasso", trControl=control, tuneLength=5)
ratio_lasso_preds <- predict(ratio_lasso_model,pentaTest)
ratio_lasso_RMSE <- rmse(pentaTest[,20],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,20],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,20],ratio_lasso_preds)

# Try different Coefficients of Lasso Model and Check which coefficients produce best results

coefs <- predict.lars(ratio_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
which.min(ratio_lasso_model$finalModel$Cp )
winnermodelscoeffs <- models[15,] # Find the best model
ratio_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(ratio_lasso_cfs)))




# FOR UC ----

ucs <- read.csv("results/ucs_complete.csv")
ucs <- ucs[, c(25:31,35)] %>%  # 7 features + last col outcome
  na.omit() %>%
  scale() %>%
  as.data.frame()

set.seed(1)
data_split = sample.split(ucs, SplitRatio = 0.8)
pentaTrain <- subset(ucs, data_split == TRUE)
pentaTest <-subset(ucs, data_split == FALSE)

## Feature Selection ----

### RFE Feature Selection ----

rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
results <- rfe(pentaTrain[,1:7], pentaTrain[,8],sizes=c(1:7), rfeControl=rfcontrol)

# summarize the results

print(results)
predictors(results)

### Boruta Selection ----

boruta_output <- Boruta(TotalOutreachCoverage ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)


## Lasso Model ----

ratio_lasso_model <- train(TotalOutreachCoverage~., data=pentaTrain[,c(1:4,7,8)], method="lasso", trControl=control, tuneLength=5)
ratio_lasso_preds <- predict(ratio_lasso_model,pentaTest)
ratio_lasso_RMSE <- rmse(pentaTest[,8],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,8],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,8],ratio_lasso_preds)

coefs <- predict.lars(ratio_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
ratio_lasso_model$finalModel$Cp 
winnermodelscoeffs <- models[6,] # Find the best model
ratio_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(ratio_lasso_cfs)))




### test pearson's r between poverty and proportion
lmod <- lm(TotalOutreachCoverage ~ ., data = tehsils.plot[,-c(1:2)])
summary(lmod)
