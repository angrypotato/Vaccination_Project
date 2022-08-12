# tehsils <- read.csv("results/tehsil_edu.csv")[,-1]
tehsils.ratio <-  tehsils[,c(1:3,7:10,12,16,24,20)] %>%  
  scale() %>%
  as.data.frame()

tehsils.ratio <- tehsils.ratio[complete.cases(tehsils.ratio),]

### Split Tehsil data into train and test set

set.seed(43)
data_split = sample.split(tehsils.ratio, SplitRatio = 0.8)
pentaTrain <- subset(tehsils.ratio, data_split == TRUE)
pentaTest <-subset(tehsils.ratio, data_split == FALSE)



std_mean <- function(x) sd(x)/sqrt(length(x))


## GBM ----



#### SE ----
set.seed(0)

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, 
                    "child_population"=NA, "population_density"=NA,"radio"=NA, "electricity"=NA,"mobile_phone"=NA, 
                    "mothers_age"=NA, "Primary"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x = c(1:10), 
    gbm.y = 11,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.005,
    bag.fraction = 0.5,
    cv_folds = 10,
  )
  
  gbm_pred = predict(clinic.step,pentaTest)
  rmse <- rmse(pentaTest[,11],gbm_pred)
  r2 <- R2(pentaTest[,11],gbm_pred)
  mae <- mae(pentaTest[,11],gbm_pred)
  
  
  ## fill in the blank list
  
  gbm_cfs <- summary(clinic.step)
  sing.mod <- data.frame(matrix(ncol = 10, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  coefs <- rbind(coefs, sing.mod)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_final <- data.frame( "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)),
                          "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)),
                          "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)),
                          "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                          "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)),
                          "radio"=c(mean(coefs$radio),std_mean(coefs$radio)),  
                          "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                          "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),
                          "mothers_age"=c(mean(coefs$mothers_age),std_mean(coefs$mothers_age)),  
                          "Primary"=c(mean(coefs$Primary),std_mean(coefs$Primary)))
View(t(coef_final))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




## GAM ----

library(mgcv)


#### SE ----
set.seed(0)

gam.form <- as.formula(OutreachProportion ~  s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + 
                         s(child_population, k=5) + s(population_density, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(mobile_phone, k=5) +  s(mothers_age, k=5) + s(Primary, k=5))

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, 
                    "child_population"=NA, "population_density"=NA, "radio"=NA,  "electricity"=NA,
                    "mobile_phone"=NA, "mothers_age"=NA, "Primary"=NA)

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  gam.mod <- gam(gam.form, data = sample_d, method = "REML") 
  
  gam_preds <- predict(gam.mod, pentaTest)
  rmse <- rmse(pentaTest[,11],gam_preds)
  r2 <- R2(pentaTest[,11],gam_preds)
  mae <- mae(pentaTest[,11],gam_preds)
  
  
  ## fill in the blank list
  
  clinic_gam_summary <- summary(gam.mod$finalModel)
  clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  clinic_gam_cfs  <- as.data.frame(t(clinic_gam_cfs))
  names(clinic_gam_cfs) <- c("fertility", "elevation", "poverty", "child_population", "population_density", "radio",  "electricity",
                             "mobile_phone", "mothers_age", "Primary")
  
  coefs <- rbind(coefs, clinic_gam_cfs)
  
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

coefs <- coefs[-1,]

coef_clean <- coefs[is.finite(rowSums(coefs)),]
coef_final <- data.frame( "fertility"=c(mean(coef_clean$fertility), std_mean(coef_clean$fertility)),
                          "elevation"=c(mean(coef_clean$elevation), std_mean(coef_clean$elevation)),
                          "poverty"=c(mean(coef_clean$poverty), std_mean(coef_clean$poverty)),
                          "child_population"=c(mean(coef_clean$child_population), std_mean(coef_clean$child_population)),
                          "population_density"=c(mean(coef_clean$population_density),std_mean(coef_clean$population_density)), 
                          "radio"=c(mean(coef_clean$radio),std_mean(coef_clean$radio)),  
                          "electricity"=c(mean(coef_clean$electricity), std_mean(coef_clean$electricity)),
                          "mobile_phone"=c(mean(coef_clean$mobile_phone),std_mean(coef_clean$mobile_phone)),
                          "mothers_age"=c(mean(coef_clean$mothers_age),std_mean(coef_clean$mothers_age)),  
                          "Primary"=c(mean(coef_clean$Primary),std_mean(coef_clean$Primary)))

View(t(coef_final))

mod_clean <- mod_performance[is.finite(rowSums(coefs)),]
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))





## Ridge ----


library(glmnet)


#### SE ----
set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "child_population"=rep(0, 1000), 
                    "population_density"=rep(0, 1000), "radio"=rep(0, 1000), "electricity"=rep(0, 1000),"mobile_phone"=rep(0, 1000),"mothers_age"=rep(0, 1000),
                    "Primary"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$OutreachProportion
  x <- data.matrix(sample_d[, c(1:10)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1:10)]))
  rmse <- rmse(pentaTest[,11],preds)
  r2 <- R2(pentaTest[,11],preds)
  mae <- MAE(pentaTest[,11],preds)
  
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
                         "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                         "population_density"=c(mean(coefs$population_density),std_mean(coefs$population_density)), 
                         "radio"=c(mean(coefs$radio),std_mean(coefs$radio)),  
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),
                         "mobile_phone"=c(mean(coefs$mobile_phone),std_mean(coefs$mobile_phone)),
                         "mothers_age"=c(mean(coefs$mothers_age),std_mean(coefs$mothers_age)),  
                         "Primary"=c(mean(coefs$Primary),std_mean(coefs$Primary)))

View(t(coef_final))

data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))

