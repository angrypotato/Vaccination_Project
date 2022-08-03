# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
# in Clinic Pentavalent Vacc Coverage
# 2.  Develop Models Using Such Covariates in Order to Predict this In Clinic 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  

source(file='PreRunNew.r')



# For Tehsil ----

### Take the existing Tehsil level data with covariates and Vaccination ratios and parse out the 
### covariates from the Y (Clinic Vaccination Coverage)

# tehsils <- read.csv("results/tehsils_complete_7.19.csv")
tehsils.clinic <- tehsils[,c(3:21,24,27)] %>%   # 19 features + last col the outcome
  scale() %>%
  as.data.frame() 

tehsils.clinic <- tehsils.clinic[complete.cases(tehsils.clinic[,-4]), -4]  ### 132 obs  ### 7/21 using this



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


clinic.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:3,5:12,16,18,19),   # selected features 
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(clinic.step,pentaTest,3100)
gbm_rmse <- rmse(pentaTest[,20],gbm_pred)
gbm_rsquared <- R2(pentaTest[,20],gbm_pred)
gbm_mae <- mae(pentaTest[,20],gbm_pred)

### What features did the GBM model identify as significant in predicting our Y?

gbm_cfs <- summary(clinic.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))



### GAM ----


### Evaluate Performances of GAM Model using RMSE,R2,MAE


library(mgcv)

gam.form <- as.formula(TotalClinicsCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) +  s(distance_to_cities, k=5) +
                         s(Population, k=5) + s(child_population, k=5) + s(population_density, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(television, k=5) + s(mobile_phone, k=5) + s(mothers_age, k=5) +
                         s(urban_to_rural, k=5) + s(fac_number, k=5))

gam.mod <- gam(gam.form, data = pentaTrain, method = "REML")  

gam_preds <- predict(gam.mod, pentaTest)

clinic_gam_RMSE <- rmse(pentaTest[,20],gam_preds)
clinic_gam_R2 <- R2(pentaTest[,20],gam_preds)
clinic_gam_MAE <- MAE(pentaTest[,20],gam_preds)

clinic_gam_summary <- summary(gam.mod$finalModel)
clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
xtable(data.frame(clinic_gam_cfs))






### LASSO ----

### Producing the Lasso Model

control <- trainControl(method="repeatedcv", 
                        number=10,   # k for k-fold CV
                        repeats=3,   
                        search="grid")   # grid search CV 

clinic_lasso_model <- train(TotalClinicsCoverage~., data=pentaTrain[,c(1:3,5:12,16,18,19,20)], method="lasso", trControl=control, tuneLength=5)
clinic_lasso_preds <- predict(clinic_lasso_model, newdata=pentaTest)
clinic_lasso_RMSE <- rmse(pentaTest[,20],clinic_lasso_preds)
clinic_lasso_R2 <- R2(pentaTest[,20],clinic_lasso_preds)
clinic_lasso_MAE <- MAE(pentaTest[,20],clinic_lasso_preds)

# Try different Coefficients of Lasso Model and Check which coefficients produce best results

coefs <- predict.lars(clinic_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
# which.min(clinic_lasso_model$finalModel$Cp) 
winnermodelscoeffs <- models[16,] # the best model (smallest Cp)
clinic_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(clinic_lasso_cfs)))


#### using glmnet package
library(glmnet)

y <- pentaTrain$TotalClinicsCoverage
x <- data.matrix(pentaTrain[, c(1:3,5:12,16,18,19)])

cv_model <- cv.glmnet(x, y, alpha = 1)

best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


#### ridge
library(glmnet)

y <- pentaTrain$TotalClinicsCoverage
x <- data.matrix(pentaTrain[, c(1:3,5:12,16,18,19)])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min
best_lambda

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(ridge_best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))


clinic_lasso_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1:3,5:12,16,18,19)]))
clinic_lasso_RMSE <- rmse(pentaTest[,20],clinic_lasso_preds)
clinic_lasso_R2 <- R2(pentaTest[,20],clinic_lasso_preds)
clinic_lasso_MAE <- MAE(pentaTest[,20],clinic_lasso_preds)



# FOR UC ----

# ucs <- read.csv("results/uc_complete_clean.csv")
ucs <- ucs[, c(5:11,14)] %>%  # 7 features + last col outcome
  na.omit() %>%
  scale() %>%
  as.data.frame()

set.seed(0)
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

clinic_lasso_model <- train(TotalClinicsCoverage~ ., data=pentaTrain, method="lasso", trControl=control, tuneLength=5)
clinic_lasso_preds <- predict(clinic_lasso_model,pentaTest)
clinic_lasso_RMSE <- rmse(pentaTest[,8],clinic_lasso_preds)
clinic_lasso_R2 <- R2(pentaTest[,8],clinic_lasso_preds)
clinic_lasso_MAE <- MAE(pentaTest[,8],clinic_lasso_preds)

coefs <- predict.lars(clinic_lasso_model$finalModel,type="coefficients")
models <- as.data.frame(coefs$coefficients)
# which.min(clinic_lasso_model$finalModel$Cp)
winnermodelscoeffs <- models[3,] # Find the best model (smallest Cp)
clinic_lasso_cfs <- abs(winnermodelscoeffs) 
xtable(data.frame(t(clinic_lasso_cfs)))

View(data.frame(t(winnermodelscoeffs)))


## Ridge ----

y <- pentaTrain$TotalClinicsCoverage
x <- data.matrix(pentaTrain[, c(3,4,7)])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min
best_lambda

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(ridge_best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))


clinic_ridge_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(3,4,7)]))
clinic_ridge_RMSE <- rmse(pentaTest[,8],clinic_ridge_preds)
clinic_ridge_R2 <- R2(pentaTest[,8],clinic_ridge_preds)
clinic_ridge_MAE <- MAE(pentaTest[,8],clinic_ridge_preds)

