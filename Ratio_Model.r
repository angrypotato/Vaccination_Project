
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


# For Tehsil ----

### Split Data

### Take the existing Tehsil level data with covariates and Vaccination ratios and parse out the 
### covariates from the Y (Outreach/Clinic Vaccination Ratio)


# tehsils <- read.csv("results/tehsils_complete_8.15.csv")
tehsils.ratio <- tehsils[,c(4:18,20,22,25,30,26)] %>%   # 19 features + last col the outcome
  scale() %>%
  as.data.frame()

tehsils.ratio <- tehsils.ratio[complete.cases(tehsils.ratio[,-4]), -4]

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
### after removing night_lights: c(1:3,6:12,14,15)

## GBM ----

### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation

ratio.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:3,7:12,14,15),
  gbm.y = 19,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(ratio.step,pentaTest)
gbm_rmse <- rmse(pentaTest[,19],gbm_pred)
gbm_rsquared <- R2(pentaTest[,19],gbm_pred)
gbm_mae <- mae(pentaTest[,19],gbm_pred)

### What features did the GBM model identify as significant in predicting our Y (Outreach/Clinic Vacc ratio)?

gbm_cfs <- summary(ratio.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))



#### SE ----
set.seed(0)

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, 
                    "child_population"=NA,"population_density"=NA,  "radio"=NA, "electricity"=NA,"television"=NA, "mobile_phone"=NA,"antenatal_care"=NA,
                    "mothers_age"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x =  c(1:3,7:12,14,15),   # selected features 
    gbm.y = 19,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.005,
    bag.fraction = 0.5,
    cv_folds = 10,
  )
  
  gbm_pred = predict(clinic.step,pentaTest)
  rmse <- rmse(pentaTest[,19],gbm_pred)
  r2 <- R2(pentaTest[,19],gbm_pred)
  mae <- mae(pentaTest[,19],gbm_pred)
  
  
  ## fill in the blank list
  
  gbm_cfs <- summary(clinic.step)
  sing.mod <- data.frame(matrix(ncol = 11, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_final <- data.frame("fertility"=c(mean(coefs$fertility),std_mean(coefs$fertility)),
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)),
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)),
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                         "population_density"=c(mean(coefs$population_density),std_mean(coefs$population_density)),  
                         "radio"=c(mean(coefs$radio),std_mean(coefs$radio)),  
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "television"=c(mean(coefs$television), std_mean(coefs$television)), 
                         "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)),
                         "mothers_age"=c(mean(coefs$mothers_age), std_mean(coefs$mothers_age)))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))






## GAM ----

# Attain Evaluation Metrics for performance of model using covariates selected

# Define Params for Cross Validation and Grid Search - will be used in modeling



### Producing the GAM Model - tune with K-Folds Validation,Grid Search

# names(pentaTrain)[c(1:3,6:12,14,15)]

gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5) + s(elevation, k=5)  + s(poverty, k=5) + 
                         s(Population, k=5) + s(child_population, k=5)  + s(population_density,k=5) +
                         s(radio, k=5) + s(electricity, k=5) + s(television, k=5)  + s(mobile_phone, k=5) + s(antenatal_care, k=5) + s(mothers_age, k=5) )

ratio_gam_model <- gam(gam.form, data = pentaTrain, method = "REML") 

ratio_gam_preds <- predict(ratio_gam_model,pentaTest)
ratio_gam_RMSE <- rmse(pentaTest[,19],ratio_gam_preds)
ratio_gam_R2 <- R2(pentaTest[,19],ratio_gam_preds)
ratio_gam_MAE <- MAE(pentaTest[,19],ratio_gam_preds)

ratio_gam_summary <- summary(ratio_gam_model)
ratio_gam_cfs <- -log10(as.data.frame(ratio_gam_summary$s.table)['p-value'])
xtable(data.frame(ratio_gam_cfs))
View(data.frame(ratio_gam_cfs))


#### SE ----
set.seed(0)

gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5) + s(elevation, k=5)  + s(poverty, k=5) + 
                         s(Population, k=5) + s(child_population, k=5)  + s(population_density,k=5) +
                         s(radio, k=5) + s(electricity, k=5) + s(television, k=5)  + s(mobile_phone, k=5) + s(antenatal_care, k=5) + s(mothers_age, k=5) )

coefs <- data.frame("fertility"=NA,"elevation"=NA, "poverty"=NA, "Population"=NA, "child_population"=NA, "population_density"=NA, 
                    "radio"=NA,  "electricity"=NA,"television"=NA, "mobile_phone"=NA, "antenatal_care"=NA, "mothers_age"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  gam.mod <- gam(gam.form, data = sample_d, method = "REML") 
  
  gam_preds <- predict(gam.mod, pentaTest)
  rmse <- rmse(pentaTest[,19],gam_preds)
  r2 <- R2(pentaTest[,19],gam_preds)
  mae <- mae(pentaTest[,19],gam_preds)
  
  
  ## fill in the blank list
  
  clinic_gam_summary <- summary(gam.mod$finalModel)
  clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  clinic_gam_cfs  <- as.data.frame(t(clinic_gam_cfs))
  names(clinic_gam_cfs) <- c("fertility","elevation" ,"poverty","Population","child_population","population_density", "radio",  "electricity", "television" ,
                             "mobile_phone", "antenatal_care","mothers_age")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame( "fertility"=c(mean(coef_clean$fertility),std_mean(coef_clean$fertility)), 
                          "elevation"=c(mean(coef_clean$elevation),std_mean(coef_clean$elevation)), 
                          "poverty"=c(mean(coef_clean$poverty), std_mean(coef_clean$poverty)),
                          "population"=c(mean(coef_clean$Population), std_mean(coef_clean$Population)), 
                          "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                          "population_density"=c(mean(coef_clean$population_density), std_mean(coef_clean$population_density)), 
                          "radio"=c(mean(coef_clean$radio),std_mean(coef_clean$radio)),  
                          "electricity"=c(mean(coef_clean$electricity), std_mean(coef_clean$electricity)),
                          "television"=c(mean(coef_clean$television), std_mean(coef_clean$television)),
                          "mobile_phone"=c(mean(coef_clean$mobile_phone),std_mean(coef_clean$mobile_phone)),  
                          "antenatal_care"=c(mean(coef_clean$antenatal_care),std_mean(coef_clean$antenatal_care)), 
                          "mothers_age"=c(mean(coef_clean$mothers_age),std_mean(coef_clean$mothers_age)))


mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))







## ridge Model ----

y <- pentaTrain$OutreachProportion
x <- data.matrix(pentaTrain[,c(1,3,6:9,11,12,14)])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min
best_lambda

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))

ratio_ridge_preds <- predict(best_model, newx=data.matrix(pentaTest[,c(1,3,6:9,11,12,14)]))
ratio_ridge_RMSE <- rmse(pentaTest[,19],ratio_ridge_preds)
ratio_ridge_R2 <- R2(pentaTest[,19],ratio_ridge_preds)
ratio_ridge_MAE <- MAE(pentaTest[,19],ratio_ridge_preds)


#### SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000),  "poverty"=rep(0, 1000), 
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "population_density"=rep(0, 1000), "radio"=rep(0, 1000), "television"=rep(0, 1000),
                    "mobile_phone"=rep(0, 1000), "antenatal_care"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$OutreachProportion
  x <- data.matrix(sample_d[, c(1,3,6:9,11,12,14)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
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
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$OutreachProportion
  x <- data.matrix(sample_d[, -8])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda =23)
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



# adjusted r-squared ----

adj_r2 <- function(r.squared, n, p) {
  return (1 - (1 - r.squared) * ((n - 1)/(n-p-1)))
}

adj_r2()
