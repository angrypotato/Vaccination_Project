
# Purpose: 
          
# 1. Find Which Features Are the Most Significant in Predicting 
# the Ratio of Outreach Pentavalent Vaccinations to In Clinic Pentavalent Vaccination By Tehsil. 
# This is to say What features predict that more outreach vaccinations will need to be administered in 
# an area vs. those administered at clinics?
# 2.  Develop Models Using Such Covariates in Order to Predict this Ratio of Outreach / In Clinic 
# Pentavalent Vaccinations - Feature selection using RFE, Boruta.  Predictive Model using GBM, Lasso and GAM.  

source(file='PreRunNew.r')


# For Tehsil ----

### Split Data

### Take the existing Tehsil level data with covariates and Vaccination ratios and parse out the 
### covariates from the Y (Outreach/Clinic Vaccination Ratio)


# tehsils <- read.csv("results/tehsils_complete_7.19.csv")
tehsils.ratio <- tehsils[,c(3:5, 7:21,24,25)] %>%   # 19 features + last col the outcome
  scale() %>%
  as.data.frame() %>%
  na.omit()

### Split into train and test set

set.seed(1)

data_split = sample.split(tehsils.ratio, SplitRatio = 0.8)
pentaTrain <- subset(tehsils.ratio, data_split == TRUE)
pentaTest <-subset(tehsils.ratio, data_split == FALSE)


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



## GBM ----

### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation

ratio.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:3,6:10,12,16),
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

### Carve out Predictions and Test the Model
### Find the RMSE, R2, MAE metrics for evaluating the model

gbm_pred = predict(ratio.step,pentaTest,1350)
gbm_rmse <- rmse(pentaTest[,20],gbm_pred)
gbm_rsquared <- R2(pentaTest[,20],gbm_pred)
gbm_mae <- mae(pentaTest[,20],gbm_pred)

### What features did the GBM model identify as significant in predicting our Y (Outreach/Clinic Vacc ratio)?

gbm_cfs <- summary(ratio.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))



## GAM ----

# Attain Evaluation Metrics for performance of model using covariates selected

# Define Params for Cross Validation and Grid Search - will be used in modeling



### Producing the GAM Model - tune with K-Folds Validation,Grid Search

library(mgcv)

# names(pentaTrain)[c(1:3,6:10,12,16)]

gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + 
                         s(Population, k=5) + s(child_population, k=5) + s(population_density, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(mobile_phone, k=5) +  s(mothers_age, k=5) )

ratio_gam_model <- gam(gam.form, data = pentaTrain, method = "REML") 

ratio_gam_preds <- predict(ratio_gam_model,pentaTest)
ratio_gam_RMSE <- rmse(pentaTest[,20],ratio_gam_preds)
ratio_gam_R2 <- R2(pentaTest[,20],ratio_gam_preds)
ratio_gam_MAE <- MAE(pentaTest[,20],ratio_gam_preds)

ratio_gam_summary <- summary(ratio_gam_model)
ratio_gam_cfs <- -log10(as.data.frame(ratio_gam_summary$s.table)['p-value'])
xtable(data.frame(ratio_gam_cfs))
View(data.frame(ratio_gam_cfs))



## Lasso Model ----

control <- trainControl(method="repeatedcv", number=10, repeats=3,search="grid")
ratio_lasso_model <- train(OutreachProportion~., data=pentaTrain[,c(1:3,6:10,12,16,20)], method="lasso", trControl=control, tuneLength=5)  ### metric = "Rsquared"
ratio_lasso_preds <- predict(ratio_lasso_model,pentaTest) 
ratio_lasso_RMSE <- rmse(pentaTest[,20],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,20],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,20],ratio_lasso_preds)

# Try different Coefficients of Lasso Model and Check which coefficients produce best results

coefs <- predict.lars(ratio_lasso_model$finalModel,type="coefficients",Mode="lambda")   ### Mode="lambda"
models <- as.data.frame(coefs$coefficients)
which.min(ratio_lasso_model$finalModel$Cp)

winnermodelscoeffs <- models[4,] # Find the best model

ratio_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(ratio_lasso_cfs)))
View(data.frame(t(winnermodelscoeffs)))


### manually choose
manuallychosen <- models[7,]
ratio_lasso_cfs <- abs(manuallychosen) 
xtable(data.frame(t(ratio_lasso_cfs)))
View(data.frame(t(manuallychosen)))


#### method 2
library(glmnet)

y <- pentaTrain$OutreachProportion
x <- data.matrix(pentaTrain[,c(1:3,6:10,12,16)])

cv_model <- cv.glmnet(x, y, alpha = 1)

best_lambda <- cv_model$lambda.min

best_lambda

plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

coef(best_model)




#### ridge

library(glmnet)
y <- pentaTrain$OutreachProportion
x <- data.matrix(pentaTrain[,c(1:3,6:10,12,16)])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))

ratio_ridge_preds <- predict(best_model, newx=data.matrix(pentaTest[,c(1:3,6:10,12,16)]))
ratio_ridge_RMSE <- rmse(pentaTest[,20],ratio_ridge_preds)
ratio_ridge_R2 <- R2(pentaTest[,20],ratio_ridge_preds)
ratio_ridge_MAE <- MAE(pentaTest[,20],ratio_ridge_preds)





# FOR UC ----

ucs <- read.csv("results/uc_complete_clean.csv")
ucs <- ucs[, c(5:12)] %>%  # 7 features + last col outcome
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

boruta_output <- Boruta(OutreachProportion ~ ., data=pentaTrain, doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

plot(boruta_output, cex = .5,cex.main = .7,font.axis=.3, cex.axis=.5, las=1, xlab="Covariate", main="Variable Importance")  # plot variable importance
outreach_df <- attStats(boruta_output)


## Lasso Model ----

ratio_lasso_model <- train(OutreachProportion~., data=pentaTrain, method="lasso", trControl=control, tuneLength=5)
ratio_lasso_preds <- predict(ratio_lasso_model,pentaTest)
ratio_lasso_RMSE <- rmse(pentaTest[,8],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,8],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,8],ratio_lasso_preds)

coefs <- predict.lars(ratio_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
which.min(ratio_lasso_model$finalModel$Cp)
winnermodelscoeffs <- models[8,] # Find the best model
ratio_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(ratio_lasso_cfs)))
View(data.frame(t(winnermodelscoeffs)))


## ridge ----

library(glmnet)

y <- pentaTrain$OutreachProportion
x <- data.matrix(pentaTrain[, -8])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min
best_lambda

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(ridge_best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))


ratio_lasso_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-8]))
ratio_lasso_RMSE <- rmse(pentaTest[,8],ratio_lasso_preds)
ratio_lasso_R2 <- R2(pentaTest[,8],ratio_lasso_preds)
ratio_lasso_MAE <- MAE(pentaTest[,8],ratio_lasso_preds)




### test pearson's r between poverty and proportion
lmod <- lm(OutreachProportion ~ ., data = tehsils.plot[,-c(2:3)])
summary(lmod)
