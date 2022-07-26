# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
#  Pentavalent Vacc Coverage via outreach program
# 2.  Develop Models Using Such Covariates in Order to Predict this outreach 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  

source(file='VaccinationStudy/PreRun.r')


# Tehsil ----

### Split Data

set.seed(43)

### Take the existing Tehsil level data with covariates and Vaccination outreachs and parse out the 
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

library(Boruta)

set.seed(5)

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

outreach.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:3,5:7,9,10,14,19),
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(outreach.step,pentaTest, 550)
gbm_rmse <- rmse(pentaTest[,20],gbm_pred)
gbm_rsquared <- R2(pentaTest[,20],gbm_pred)
gbm_mae <- mae(pentaTest[,20],gbm_pred)

# Get the relative influences provided by GBM to see which features are being most utilized by the model

gbm_cfs <- summary(outreach.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))




## GAM ----

library(mgcv)

gam.form <- as.formula(TotalOutreachCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + 
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(antenatal_care, k=5) + s(fac_number, k=5))

outreach_gam_model <- gam(gam.form, data = pentaTrain, method = "REML") 
 
outreach_gam_preds <- predict(outreach_gam_model,pentaTest)

### Evaluate Performances of GAM Model using RMSE,R2,MAE

outreach_gam_RMSE <- rmse(pentaTest[,20],outreach_gam_preds)
outreach_gam_R2 <- R2(pentaTest[,20],outreach_gam_preds)
outreach_gam_MAE <- MAE(pentaTest[,20],outreach_gam_preds)
outreach_gam_summary <- summary(outreach_gam_model)
outreach_gam_cfs <- -log10(as.data.frame(outreach_gam_summary$s.table)['p-value'])
xtable(data.frame(outreach_gam_cfs))




## LASSO ----

#### method 1

control <- trainControl(method="repeatedcv", number=10, repeats=3,search="grid")
outreach_lasso_model <- train(TotalOutreachCoverage~., data=pentaTrain[,c(1:3,5:7,9,10,14,19,20)], 
                           method="lasso", trControl=control, tuneLength=5)
outreach_lasso_preds <- predict(outreach_lasso_model,pentaTest)
outreach_lasso_RMSE <- rmse(pentaTest[,20],outreach_lasso_preds)
outreach_lasso_R2 <- R2(pentaTest[,20],outreach_lasso_preds)
outreach_lasso_MAE <- MAE(pentaTest[,20],outreach_lasso_preds)

# Try different Coefficients of Lasso Model and Check which coefficients produce best results

coefs <- predict.lars(outreach_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
which.min(outreach_lasso_model$finalModel$Cp )
winnermodelscoeffs <- models[10,] # Find the best model
outreach_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(outreach_lasso_cfs)))


#### method 2
library(glmnet)

y <- pentaTrain$TotalOutreachCoverage
x <- data.matrix(pentaTrain[, c(1:3,5:7,9,10,14,19)])

cv_model <- cv.glmnet(x, y, alpha = 1)

best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

coef(best_model)








# FOR UC ----

ucs <- read.csv("results/uc_complete_clean.csv")
ucs <- ucs[, c(5:11, 13)] %>%  # 7 features + last col outcome
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

set.seed(1)

boruta_output <- Boruta(TotalOutreachCoverage ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)


## Lasso Model ----

outreach_lasso_model <- train(TotalOutreachCoverage~., data=pentaTrain, method="lasso", trControl=control, tuneLength=5)
outreach_lasso_preds <- predict(outreach_lasso_model,pentaTest)
outreach_lasso_RMSE <- rmse(pentaTest[,8],outreach_lasso_preds)
outreach_lasso_R2 <- R2(pentaTest[,8],outreach_lasso_preds)
outreach_lasso_MAE <- MAE(pentaTest[,8],outreach_lasso_preds)

coefs <- predict.lars(outreach_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
outreach_lasso_model$finalModel$Cp 
winnermodelscoeffs <- models[8,] # Find the best model
outreach_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(outreach_lasso_cfs)))
View(data.frame(t(winnermodelscoeffs)))



### test pearson's r between poverty and proportion
lmod <- lm(TotalOutreachCoverage ~ ., data = tehsils.plot[,-c(1:2)])
summary(lmod)
