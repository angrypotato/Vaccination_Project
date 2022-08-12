# Purpose: 

# 1. Find Which Covariates Are the Most Significant in Predicting 
#  Pentavalent Vacc Coverage via outreach program
# 2.  Develop Models Using Such Covariates in Order to Predict this outreach 
# Pentavalent Vaccination Rate - Feature selection using RFE, Boruta.  Predictive Modeling using GBM, Lasso and GAM.  

source(file='PreRunNew.r')


# Tehsil ----

### Split Data


### Take the existing Tehsil level data with covariates and Vaccination outreachs and parse out the 
### covariates from the Y (Outreach Vaccination Coverage)

# tehsils <- read.csv("results/tehsils_complete_7.19.csv")
tehsils.outreach <-  tehsils[,c(3:5, 7:21,24,26)] %>%   # 19 features + last col the outcome
  scale() %>%
  as.data.frame()

tehsils.outreach <- tehsils.outreach[complete.cases(tehsils.outreach),]

### Split Tehsil data into train and test set

set.seed(43)
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


std_mean <- function(x) sd(x)/sqrt(length(x))


## GBM ----

### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation

outreach.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(2,3,5,7,9,10,14),
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(outreach.step,pentaTest, 500)
gbm_rmse <- rmse(pentaTest[,20],gbm_pred)
gbm_rsquared <- R2(pentaTest[,20],gbm_pred)
gbm_mae <- mae(pentaTest[,20],gbm_pred)

# Get the relative influences provided by GBM to see which features are being most utilized by the model

gbm_cfs <- summary(outreach.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))


#### SE ----
set.seed(0)

coefs <- data.frame("elevation"=NA, "poverty"=NA, "distance_to_cities"=NA, 
                    "child_population"=NA,"radio"=NA, "electricity"=NA,"antenatal_care"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x = c(2,3,5,7,9,10,14),   # selected features 
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
  sing.mod <- data.frame(matrix(ncol = 7, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_final <- data.frame("child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)),
                         "radio"=c(mean(coefs$radio),std_mean(coefs$radio)),  
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)),
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)),
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)))
                         

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




## GAM ----

library(mgcv)

gam.form <- as.formula(TotalOutreachCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + 
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(antenatal_care, k=5) + s(fac_number, k=5))

outreach_gam_model <- gam(gam.form, data = pentaTrain, method = "REML") 

outreach_gam_preds <- predict(outreach_gam_model,pentaTest)
outreach_gam_RMSE <- rmse(pentaTest[,20],outreach_gam_preds)
outreach_gam_R2 <- R2(pentaTest[,20],outreach_gam_preds)
outreach_gam_MAE <- MAE(pentaTest[,20],outreach_gam_preds)

outreach_gam_summary <- summary(outreach_gam_model)
outreach_gam_cfs <- -log10(as.data.frame(outreach_gam_summary$s.table)['p-value'])
xtable(data.frame(outreach_gam_cfs))

#### SE ----
set.seed(0)

gam.form <- as.formula(TotalOutreachCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + 
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(antenatal_care, k=5) + s(fac_number, k=5))

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, "distance_to_cities"=NA, "Population"=NA,
                    "child_population"=NA,  "radio"=NA,  "electricity"=NA, "antenatal_care"=NA, "fac_number"=NA)

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
  names(clinic_gam_cfs) <- c("fertility", "elevation", "poverty", "distance_to_cities", "Population",
                             "child_population",  "radio",  "electricity", "antenatal_care", "fac_number")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame( "fertility"=c(mean(coef_clean$fertility),std_mean(coef_clean$fertility)), 
                          "elevation"=c(mean(coef_clean$elevation), std_mean(coef_clean$elevation)),
                          "poverty"=c(mean(coef_clean$poverty), std_mean(coef_clean$poverty)),
                          "distance_to_cities"=c(mean(coef_clean$distance_to_cities), std_mean(coef_clean$distance_to_cities)),
                          "population"=c(mean(coef_clean$Population), std_mean(coef_clean$Population)), 
                          "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                          "radio"=c(mean(coef_clean$radio),std_mean(coef_clean$radio)),  
                          "electricity"=c(mean(coef_clean$electricity), std_mean(coef_clean$electricity)),
                          "antenatal_care"=c(mean(coef_clean$antenatal_care),std_mean(coef_clean$antenatal_care)),  
                          "fac_number"=c(mean(coef_clean$fac_number),std_mean(coef_clean$fac_number)))
                         
                         
mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))





## Ridge ----


library(glmnet)

y <- pentaTrain$TotalOutreachCoverage
x <- data.matrix(pentaTrain[, c(1:3,5:7,9,10,14,19)])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min
best_lambda

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(ridge_best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))


outreach_ridge_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1:3,5:7,9,10,14,19)]))
outreach_ridge_RMSE <- rmse(pentaTest[,20],outreach_ridge_preds)
outreach_ridge_R2 <- R2(pentaTest[,20],outreach_ridge_preds)
outreach_ridge_MAE <- MAE(pentaTest[,20],outreach_ridge_preds)


#### SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "radio"=rep(0, 1000), "electricity"=rep(0, 1000),"antenatal_care"=rep(0, 1000),
                    "fac_number"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, c(1:3,5:7,9,10,14,19)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1:3,5:7,9,10,14,19)]))
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
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)), 
                         "fac_number"=c(mean(coefs$fac_number), std_mean(coefs$fac_number)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




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




## ridge ----

library(glmnet)

y <- pentaTrain$TotalOutreachCoverage
x <- data.matrix(pentaTrain[, -8])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min
best_lambda

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(ridge_best_model)
data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x)
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))


outreach_lasso_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-8]))
outreach_lasso_RMSE <- rmse(pentaTest[,8],outreach_lasso_preds)
outreach_lasso_R2 <- R2(pentaTest[,8],outreach_lasso_preds)
outreach_lasso_MAE <- MAE(pentaTest[,8],outreach_lasso_preds)



### test pearson's r between poverty and proportion
lmod <- lm(TotalOutreachCoverage ~ ., data = tehsils.plot[,-c(1:2)])
summary(lmod)




#### SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000))
  
mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, -8])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-8]))
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
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))


## SE With lambda fixed

set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, -8])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 70)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-8]))
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
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

plot(coefs$population_density)



### VIF for lambda selection ----

library(genridge)
 
lmod <- lm(TotalOutreachCoverage ~., data=pentaTrain)
vif(lmod)

y <- pentaTrain[, "TotalOutreachCoverage"]
X <- data.matrix(pentaTrain[, c(1:7)])

lambda <- c(0, 0.005, 0.01, 0.02, 0.04, 0.08, 1,5,10,20,30,40,50)
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

