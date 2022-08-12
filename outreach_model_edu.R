# tehsils <- read.csv("results/tehsil_edu.csv")[,-1]
tehsils.outreach <-  tehsils[,c(2,3,5:7,9,10,14,25,21)] %>%  
  scale() %>%
  as.data.frame()

tehsils.outreach <- tehsils.outreach[complete.cases(tehsils.outreach),]

### Split Tehsil data into train and test set

set.seed(43)
data_split = sample.split(tehsils.outreach, SplitRatio = 0.8)
pentaTrain <- subset(tehsils.outreach, data_split == TRUE)
pentaTest <-subset(tehsils.outreach, data_split == FALSE)



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

coefs <- data.frame("elevation"=NA, "poverty"=NA, "distance_to_cities"=NA, "Population"=NA,
                    "child_population"=NA,"radio"=NA, "electricity"=NA,"antenatal_care"=NA, "Middle"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x = c(1:9), 
    gbm.y = 10,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.005,
    bag.fraction = 0.5,
    cv_folds = 10,
  )
  
  gbm_pred = predict(clinic.step,pentaTest)
  rmse <- rmse(pentaTest[,10],gbm_pred)
  r2 <- R2(pentaTest[,10],gbm_pred)
  mae <- mae(pentaTest[,10],gbm_pred)
  
  
  ## fill in the blank list
  
  gbm_cfs <- summary(clinic.step)
  sing.mod <- data.frame(matrix(ncol = 9, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_final <- data.frame( "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)),
                          "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)),
                          "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)),
                          "population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                          "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                          "radio"=c(mean(coefs$radio),std_mean(coefs$radio)),  
                          "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                          "antenatal_care"=c(mean(coefs$antenatal_care),std_mean(coefs$antenatal_care)),  
                          "Middle"=c(mean(coefs$Middle),std_mean(coefs$Middle)))

View(t(coef_final))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




## GAM ----

library(mgcv)


#### SE ----
set.seed(0)

gam.form <- as.formula(TotalOutreachCoverage ~  s(elevation, k=5) + s(poverty, k=5) + 
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(antenatal_care, k=5) + s(Middle, k=5))

coefs <- data.frame("elevation"=NA, "poverty"=NA, "distance_to_cities"=NA, "Population"=NA,
                    "child_population"=NA,  "radio"=NA,  "electricity"=NA, "antenatal_care"=NA, "Middle"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  gam.mod <- gam(gam.form, data = sample_d, method = "REML") 
  
  gam_preds <- predict(gam.mod, pentaTest)
  rmse <- rmse(pentaTest[,10],gam_preds)
  r2 <- R2(pentaTest[,10],gam_preds)
  mae <- mae(pentaTest[,10],gam_preds)
  
  
  ## fill in the blank list
  
  clinic_gam_summary <- summary(gam.mod$finalModel)
  clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  clinic_gam_cfs  <- as.data.frame(t(clinic_gam_cfs))
  names(clinic_gam_cfs) <- c("elevation", "poverty", "distance_to_cities", "Population",
                             "child_population",  "radio",  "electricity", "antenatal_care", "Middle")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame( "elevation"=c(mean(coef_clean$elevation), std_mean(coef_clean$elevation)),
                          "poverty"=c(mean(coef_clean$poverty), std_mean(coef_clean$poverty)),
                          "distance_to_cities"=c(mean(coef_clean$distance_to_cities), std_mean(coef_clean$distance_to_cities)),
                          "population"=c(mean(coef_clean$Population), std_mean(coef_clean$Population)), 
                          "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                          "radio"=c(mean(coef_clean$radio),std_mean(coef_clean$radio)),  
                          "electricity"=c(mean(coef_clean$electricity), std_mean(coef_clean$electricity)),
                          "antenatal_care"=c(mean(coef_clean$antenatal_care),std_mean(coef_clean$antenatal_care)),  
                          "Middle"=c(mean(coef_clean$Middle),std_mean(coef_clean$Middle)))

View(t(coef_final))

mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))





## Ridge ----


library(glmnet)


#### SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000), "child_population"=rep(0, 1000), "radio"=rep(0, 1000), "electricity"=rep(0, 1000),"antenatal_care"=rep(0, 1000),
                    "Middle"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, c(1:9)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1:9)]))
  rmse <- rmse(pentaTest[,10],preds)
  r2 <- R2(pentaTest[,10],preds)
  mae <- MAE(pentaTest[,10],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}


coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                         "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                         "Population"=c(mean(coefs$Population), std_mean(coefs$Population)), 
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "antenatal_care"=c(mean(coefs$antenatal_care), std_mean(coefs$antenatal_care)), 
                         "Middle"=c(mean(coefs$Middle), std_mean(coefs$Middle)))
View(t(coef_final))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))


