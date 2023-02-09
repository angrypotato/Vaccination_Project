##### run UC level GBM and GAM models
# directly use the selected features from former results


# Import packages and data ----

## packages and functions -----
source(file='PreRunNew.r')
library(genridge)
library(Boruta)
library(mgcv)
library(glmnet)

std_mean <- function(x) sd(x)/sqrt(length(x))

test_scale <- function(raw_test, train.mean, train.sd) {
  df <- raw_test
  for (n in 1:ncol(df)) {
    df[,n] <- (raw_test[,n] - train.mean[n])/train.sd[n]
  }
  df
}

adj.r2 <- function(r2, n, p) {
  1 - (1-r2)*(n-1)/(n-p-1)
}

## data ----
ucs <- read.csv("results/uc_complete_buffer12.csv")
ucs <- ucs[, c(6:12,15:17)] %>%  # 7 features + last 3 col outcome
  na.omit() 

set.seed(1)
data_split <- sample.split(ucs, SplitRatio = 0.8)
pentaTrain.raw <- subset(ucs, data_split == TRUE) 
train.sd <- apply(pentaTrain.raw, 2, sd)
train.mean <- apply(pentaTrain.raw, 2, mean)

pentaTrain <- scale(pentaTrain.raw) %>%
  as.data.frame()

pentaTest.raw <-subset(ucs, data_split == FALSE) 

pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)


# clinic model ----

## gbm ----

set.seed(0)

clinic.gbm.coefs <- data.frame("fertility"=NA, "poverty"=NA,  "distance_to_cities"=NA)

clinic.gbm.mod_performance <- data.frame("RMSE" = rep(0, 100), "R2" = rep(0, 100), "MAE"=rep(0, 100))

for (i in 78:100) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  clinic.step <- gbm.step(
    data=sample_d, 
    gbm.x = c(1,3,4),   # selected features 
    gbm.y = 10,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.00001,
    bag.fraction = 0.5,
    cv_folds = 10,
    plot.main = F,
    verbose = F
  )
  
  gbm_pred = predict(clinic.step,pentaTest)
  rmse <- rmse(pentaTest[,10],gbm_pred)
  r2 <- R2(pentaTest[,10],gbm_pred)
  mae <- mae(pentaTest[,10],gbm_pred)
  
  
  ## fill in the blank list
  
  gbm_cfs <- summary(clinic.step)
  sing.mod <- data.frame(matrix(ncol = 3, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  clinic.gbm.coefs <- rbind(clinic.gbm.coefs, sing.mod)
  
  clinic.gbm.mod_performance[i,1] <- rmse
  clinic.gbm.mod_performance[i,2] <- r2
  clinic.gbm.mod_performance[i,3] <- mae
  
  print(i)
}

clinic.gbm.coefs <- clinic.gbm.coefs[-1,]

clinic.gbm.coef_final <- data.frame("fertility"=c(mean(clinic.gbm.coefs$fertility), std_mean(clinic.gbm.coefs$fertility)), 
                         "poverty"=c(mean(clinic.gbm.coefs$poverty), std_mean(clinic.gbm.coefs$poverty)), 
                         "distance_to_cities"=c(mean(clinic.gbm.coefs$distance_to_cities), std_mean(clinic.gbm.coefs$distance_to_cities)))

data.frame("RMSE" = mean(clinic.gbm.mod_performance$RMSE), "R2" = mean(clinic.gbm.mod_performance$R2), "MAE" = mean(clinic.gbm.mod_performance$MAE))


## gam ----
clinic.gam.form <- as.formula(TotalClinicsCoverage ~ s(fertility, k=5)  + s(poverty, k=5) +  s(distance_to_cities, k=5) )

# direction 
clinic.gam.mod <- gam(clinic.gam.form, data = pentaTrain, method = "REML") 
summary(clinic.gam.mod)
par(mfrow=c(1,3))
plot(clinic.gam.mod, )


# SE
set.seed(0)

clinic.gam.coefs <- data.frame("fertility"=NA, "poverty"=NA, "distance_to_cities"=NA)

clinic.gam.mod_performance <- data.frame("RMSE" = rep(0, 100), "R2" = rep(0, 100), "MAE"=rep(0, 100))

for (i in 1:100) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  gam.mod <- gam(clinic.gam.form, data = sample_d, method = "REML") 
  
  raw_pred = predict(gam.mod,pentaTest)
  preds <- raw_pred*train.sd[10]+train.mean[10]
  rmse <- rmse(pentaTest.raw[,10],preds)
  r2 <- R2(pentaTest.raw[,10],preds)
  mae <- MAE(pentaTest.raw[10],preds)
  
  ## fill in the blank list
  
  clinic_gam_summary <- summary(gam.mod$finalModel)
  clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  clinic_gam_cfs  <- as.data.frame(t(clinic_gam_cfs))
  names(clinic_gam_cfs) <- c("fertility","poverty","distance_to_cities")
  
  clinic.gam.coefs <- rbind(clinic.gam.coefs, clinic_gam_cfs)
  
  clinic.gam.mod_performance[i,1] <- rmse
  clinic.gam.mod_performance[i,2] <- r2
  clinic.gam.mod_performance[i,3] <- mae
  
  print(i)
}

clinic.gam.coefs <- clinic.gam.coefs[-1,]

clinic.gam.coef_clean <- clinic.gam.coefs[is.finite(rowSums(clinic.gam.coefs)),]
clinic.gam.coef_final <- data.frame( "fertility"=c(mean(clinic.gam.coef_clean$fertility),std_mean(clinic.gam.coef_clean$fertility)), 
                          "poverty"=c(mean(clinic.gam.coef_clean$poverty), std_mean(clinic.gam.coef_clean$poverty)),
                          "distance_to_cities"=c(mean(clinic.gam.coef_clean$distance_to_cities),std_mean(clinic.gam.coef_clean$distance_to_cities)))


clinic.gam.mod_clean <- clinic.gam.mod_performance[is.finite(rowSums(clinic.gam.coefs)),]
data.frame("RMSE" = mean(clinic.gam.mod_performance$RMSE), "R2" = mean(clinic.gam.mod_performance$R2), "MAE" = mean(clinic.gam.mod_performance$MAE))


## ridge ----

### linearity ----
clinic_y <- ucs$TotalClinicsCoverage[-which(ucs$TotalClinicsCoverage >1)]  # exclude outlier (coverage rate > 1)
clinic_x <- data.matrix(ucs[-which(ucs$TotalClinicsCoverage >1), c(1,3,4)])

par(mfrow = c(3,2))
for (i in 1:3) {
  plot(clinic_y ~ clinic_x[,i])
  title(main = colnames(clinic_x)[i])
  plot(clinic_y ~ log(clinic_x[,i]))
  title(main = paste0("y ~ log(",colnames(clinic_x)[i],")"))
}

### residual plots ----
# train set
log.train.raw <- pentaTrain.raw[, c(1,3,4)]
log.train.raw <- apply(log.train.raw, 2, log) %>%
  as.data.frame()
## na and inf produced, fill with mean
for (i in 1:ncol(log.train.raw)) {
  if (sum(is.na(log.train.raw[,i])) > 0) {
    log.train.raw[is.na(log.train.raw[,i]),i] <- mean(log.train.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.train.raw[,i])) > 0) {
    log.train.raw[is.infinite(log.train.raw[,i]),i] <- mean(log.train.raw[!is.infinite(log.train.raw[,i]),i],na.rm=TRUE)
  }
}
## add outcome col
log.train.raw <- cbind(log.train.raw, pentaTrain.raw[,10])
colnames(log.train.raw)[4] <- "TotalClinicsCoverage"

# fit model
sample_d <- scale(log.train.raw) %>%
  as.data.frame()
y <- sample_d$TotalClinicsCoverage
x <- data.matrix(sample_d[, -4])

ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
best_lambda <- ridge_model$lambda.min

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
ridge_outcome <- coef(ridge_best_model)

# residual plots
fitted <- predict(ridge_best_model, newx = x)
residual <- y - fitted
fitted <- fitted[-which(residual > 0.5)]  # extreme points
residual <- residual[-which(residual > 0.5)]
par(mfrow=c(1,1))
plot(fitted, residual)
title("ratio ridge model residual plot")

### fit model and estimate SE ----

# log transformation on test set
log.test.raw <- pentaTest.raw[, c(1,3,4)]
log.test.raw <- apply(log.test.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.test.raw)) {
  if (sum(is.na(log.test.raw[,i])) > 0) {
    log.test.raw[is.na(log.test.raw[,i]),i] <- mean(log.test.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.test.raw[,i])) > 0) {
    log.test.raw[is.infinite(log.test.raw[,i]),i] <- mean(log.test.raw[!is.infinite(log.test.raw[,i]),i],na.rm=TRUE)
  }
}
# add outcome col
log.test.raw <- cbind(log.test.raw, pentaTest.raw[,10])
colnames(log.test.raw)[4] <- "TotalClinicsCoverage"

# fit
set.seed(0)
coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000),  "poverty"=rep(0, 1000), 
                    "distance_to_cities"=rep(0, 1000))
mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = log.train.raw[sample(1:nrow(log.train.raw), nrow(log.train.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(log.test.raw,train.mean,train.sd)
  
  y <- sample_d$TotalClinicsCoverage
  x <- data.matrix(sample_d[, -4])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0,family = c("gaussian"), standardize = F)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-4]))
  preds <- raw_preds*train.sd[4]+train.mean[4]
  rmse <- rmse(pentaTest.raw[,10],preds)
  r2 <- R2(pentaTest.raw[,10],preds)
  mae <- MAE(pentaTest.raw[,10],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}


coef_final_log <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                              "fertility"= c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                              "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                              "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)))

metrics.log <- data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
metrics.log
View(t(coef_final_log))




# outreach model ----

## gbm ----

set.seed(0)
outreach.coefs <- data.frame("elevation"=NA, "poverty"=NA, "Population"=NA, 
                    "child_population"=NA,"population_density"=NA)
outreach.mod_performance <- data.frame("RMSE" = rep(0, 100), "R2" = rep(0, 100), "MAE"=rep(0, 100))

for (i in 1:100) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  outreach.step <- gbm.step(
    data=sample_d, 
    gbm.x = c(2,3,5:7),   # selected features 
    gbm.y = 9,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.00001,
    bag.fraction = 0.5,
    cv_folds = 10,
    plot.main = F,
    verbose = F
  )
  
  gbm_pred = predict(outreach.step,pentaTest)
  rmse <- rmse(pentaTest[,9],gbm_pred)
  r2 <- R2(pentaTest[,9],gbm_pred)
  mae <- mae(pentaTest[,9],gbm_pred)
  
  ## fill in the blank list
  
  gbm_cfs <- summary(outreach.step)
  sing.mod <- data.frame(matrix(ncol = 5, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  outreach.coefs <- rbind(outreach.coefs, sing.mod)
  
  outreach.mod_performance[i,1] <- rmse
  outreach.mod_performance[i,2] <- r2
  outreach.mod_performance[i,3] <- mae
  
  print(i)
}

outreach.coefs <- outreach.coefs[-1,]

outreach.coef_final <- data.frame("elevation"=c(mean(outreach.coefs$elevation), std_mean(outreach.coefs$elevation)), 
                         "poverty"=c(mean(outreach.coefs$poverty), std_mean(outreach.coefs$poverty)), 
                         "Population"=c(mean(outreach.coefs$Population), std_mean(outreach.coefs$Population)), 
                         "child_population"=c(mean(outreach.coefs$child_population), std_mean(outreach.coefs$child_population)), 
                         "population_density"=c(mean(outreach.coefs$population_density), std_mean(outreach.coefs$population_density)))

data.frame("RMSE" = mean(outreach.mod_performance$RMSE), "R2" = mean(outreach.mod_performance$R2), "MAE" = mean(outreach.mod_performance$MAE))



## gam ----
outreach.gam.form <- as.formula(TotalOutreachCoverage ~ s(elevation, k=5) + s(poverty, k=5) + s(Population, k=5)+ s(child_population, k=5) + s(population_density, k=5) )

# direction 
outreach.gam.mod <- gam(outreach.gam.form, data = pentaTrain, method = "REML") 
summary(outreach.gam.mod)
par(mfrow=c(1,5))
plot(outreach.gam.mod)

# SE
set.seed(0)
outreach.gam.coefs <- data.frame("elevation"=NA, "poverty"=NA, "Population"=NA, "child_population"=NA, "population_density"=NA)
outreach.gam.mod_performance <- data.frame("RMSE" = rep(0, 100), "R2" = rep(0, 100), "MAE"=rep(0, 100))

for (i in 1:100) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  gam.mod <- gam(outreach.gam.form, data = sample_d, method = "REML") 
  
  raw_pred = predict(gam.mod,pentaTest)
  preds <- raw_pred*train.sd[9]+train.mean[9]
  rmse <- rmse(pentaTest.raw[,9],preds)
  r2 <- R2(pentaTest.raw[,9],preds)
  mae <- MAE(pentaTest.raw[9],preds)
  
  ## fill in the blank list
  
  outreach_gam_summary <- summary(gam.mod$finalModel)
  outreach_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  outreach_gam_cfs  <- as.data.frame(t(outreach_gam_cfs))
  names(outreach_gam_cfs) <- c("elevation","poverty","Population","child_population","population_density")
  
  outreach.gam.coefs <- rbind(outreach.gam.coefs, outreach_gam_cfs)
  
  outreach.gam.mod_performance[i,1] <- rmse
  outreach.gam.mod_performance[i,2] <- r2
  outreach.gam.mod_performance[i,3] <- mae
  
  print(i)
}

outreach.gam.coefs <- outreach.gam.coefs[-1,]

outreach.gam.coef_clean <- outreach.gam.coefs[is.finite(rowSums(outreach.gam.coefs)),]
outreach.gam.coef_final <- data.frame( "elevation"=c(mean(outreach.gam.coef_clean$elevation),std_mean(outreach.gam.coef_clean$elevation)), 
                          "poverty"=c(mean(outreach.gam.coef_clean$poverty), std_mean(outreach.gam.coef_clean$poverty)),
                          "Population"=c(mean(outreach.gam.coef_clean$Population),std_mean(outreach.gam.coef_clean$Population)),
                          "child_population"=c(mean(outreach.gam.coef_clean$child_population),std_mean(outreach.gam.coef_clean$child_population)),
                          "population_density"=c(mean(outreach.gam.coef_clean$population_density),std_mean(outreach.gam.coef_clean$population_density)))


outreach.gam.mod_clean <- outreach.gam.mod_performance[is.finite(rowSums(outreach.gam.coefs)),]
data.frame("RMSE" = mean(outreach.gam.mod_performance$RMSE), "R2" = mean(outreach.gam.mod_performance$R2), "MAE" = mean(outreach.gam.mod_performance$MAE))



## ridge ----
clinic_y <- ucs$TotalOutreachCoverage[-which(ucs$TotalOutreachCoverage >1)]  # exclude outlier (coverage rate > 1)
clinic_x <- data.matrix(ucs[-which(ucs$TotalOutreachCoverage >1), c(2,3,5:7)])

par(mfrow = c(2,2))
for (i in 4:5) {
  plot(clinic_y ~ clinic_x[,i])
  title(main = colnames(clinic_x)[i])
  plot(clinic_y ~ log(clinic_x[,i]))
  title(main = paste0("y ~ log(",colnames(clinic_x)[i],")"))
}


### linearity ----
clinic_y <- ucs$TotalOutreachCoverage[-which(ucs$TotalOutreachCoverage >1)]  # exclude outlier (coverage rate > 1)
clinic_x <- data.matrix(ucs[-which(ucs$TotalOutreachCoverage >1), c(2,3,5:7)])

par(mfrow = c(3,2))
for (i in 1:3) {
  plot(clinic_y ~ clinic_x[,i])
  title(main = colnames(clinic_x)[i])
  plot(clinic_y ~ log(clinic_x[,i]))
  title(main = paste0("y ~ log(",colnames(clinic_x)[i],")"))
}

### residual plots ----
# train set
log.train.raw <- pentaTrain.raw[, c(2,3,5:7)]
log.train.raw <- apply(log.train.raw, 2, log) %>%
  as.data.frame()
## na and inf produced, fill with mean
for (i in 1:ncol(log.train.raw)) {
  if (sum(is.na(log.train.raw[,i])) > 0) {
    log.train.raw[is.na(log.train.raw[,i]),i] <- mean(log.train.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.train.raw[,i])) > 0) {
    log.train.raw[is.infinite(log.train.raw[,i]),i] <- mean(log.train.raw[!is.infinite(log.train.raw[,i]),i],na.rm=TRUE)
  }
}
## add outcome col
log.train.raw <- cbind(log.train.raw, pentaTrain.raw[,9])
colnames(log.train.raw)[6] <- "TotalOutreachCoverage"

# find optimal lambda
lmod <- lm(TotalOutreachCoverage ~., data=log.train.raw)
vif(lmod)

y <- log.train.raw[, "TotalOutreachCoverage"]
X <- data.matrix(log.train.raw[, -6])

lambda <- c(0, 0.005, 0.01, 0.02, 0.04, 0.08, 1,5,10,20,22,23,24,25,30,40,50)
lridge <- ridge(y,X, lambda=lambda)
coef(lridge)

vridge <- vif(lridge)
vridge
## lambda = 20

# fit model
sample_d <- scale(log.train.raw) %>%
  as.data.frame()
y <- sample_d$TotalOutreachCoverage
x <- data.matrix(sample_d[, -6])

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 20,family = c("gaussian"), standardize = F)
ridge_outcome <- coef(ridge_best_model)

# residual plots
fitted <- predict(ridge_best_model, newx = x)
residual <- y - fitted
fitted <- fitted[-which(residual > 1)]  # extreme points
residual <- residual[-which(residual > 1)]
par(mfrow=c(1,1))
plot(fitted, residual)
title("ratio ridge model residual plot")

### fit model and estimate SE ----

# log transformation on test set
log.test.raw <- pentaTest.raw[, c(2,3,5:7)]
log.test.raw <- apply(log.test.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.test.raw)) {
  if (sum(is.na(log.test.raw[,i])) > 0) {
    log.test.raw[is.na(log.test.raw[,i]),i] <- mean(log.test.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.test.raw[,i])) > 0) {
    log.test.raw[is.infinite(log.test.raw[,i]),i] <- mean(log.test.raw[!is.infinite(log.test.raw[,i]),i],na.rm=TRUE)
  }
}
# add outcome col
log.test.raw <- cbind(log.test.raw, pentaTest.raw[,9])
colnames(log.test.raw)[6] <- "TotalOutreachCoverage"

# fit
set.seed(0)
coefs <- data.frame("Intercept"= rep(0, 1000),"elevation"=rep(0, 1000), "poverty"=rep(0, 1000),
                    "Population"=rep(0, 1000),"child_population"=rep(0, 1000),"population_density"=rep(0, 1000))
mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = log.train.raw[sample(1:nrow(log.train.raw), nrow(log.train.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(log.test.raw,train.mean,train.sd)
  
  y <- sample_d$TotalOutreachCoverage
  x <- data.matrix(sample_d[, -4])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 20,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-6]))
  preds <- raw_preds*train.sd[6]+train.mean[6]
  rmse <- rmse(pentaTest.raw[,9],preds)
  r2 <- R2(pentaTest.raw[,9],preds)
  mae <- MAE(pentaTest.raw[,9],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}


coef_final_log <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                             "elevation"= c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                             "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                             "Population"=c(mean(coefs$Population), std_mean(coefs$Population)),
                             "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                             "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)))

metrics.log <- data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
metrics.log
View(t(coef_final_log))



# ratio model ----

## gbm ----
set.seed(0)
ratio.coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, "distance_to_cities"=NA,"Population"=NA, 
                             "child_population"=NA,"population_density"=NA)
ratio.mod_performance <- data.frame("RMSE" = rep(0, 100), "R2" = rep(0, 100), "MAE"=rep(0, 100))

for (i in 1:100) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  ratio.step <- gbm.step(
    data=sample_d, 
    gbm.x = c(1:7),   # selected features 
    gbm.y = 8,
    family = "gaussian",
    tree.complexity = 2,
    learning.rate = 0.00001,
    bag.fraction = 0.5,
    cv_folds = 10,
    plot.main = F,
    verbose = F
  )
  
  gbm_pred = predict(ratio.step,pentaTest)
  rmse <- rmse(pentaTest[,8],gbm_pred)
  r2 <- R2(pentaTest[,8],gbm_pred)
  mae <- mae(pentaTest[,8],gbm_pred)
  
  ## fill in the blank list
  
  gbm_cfs <- summary(ratio.step)
  sing.mod <- data.frame(matrix(ncol = 7, nrow = 0))
  names(sing.mod) <- gbm_cfs[,1]
  sing.mod[1,] <- gbm_cfs[,2]
  
  ratio.coefs <- rbind(ratio.coefs, sing.mod)
  
  ratio.mod_performance[i,1] <- rmse
  ratio.mod_performance[i,2] <- r2
  ratio.mod_performance[i,3] <- mae
  
  print(i)
}

ratio.coefs <- ratio.coefs[-1,]

ratio.coef_final <- data.frame("fertility"=c(mean(ratio.coefs$fertility), std_mean(ratio.coefs$fertility)), 
                               "elevation"=c(mean(ratio.coefs$elevation), std_mean(ratio.coefs$elevation)), 
                                  "poverty"=c(mean(ratio.coefs$poverty), std_mean(ratio.coefs$poverty)), 
                               "distance_to_cities"=c(mean(ratio.coefs$distance_to_cities), std_mean(ratio.coefs$distance_to_cities)), 
                                  "Population"=c(mean(ratio.coefs$Population), std_mean(ratio.coefs$Population)), 
                                  "child_population"=c(mean(ratio.coefs$child_population), std_mean(ratio.coefs$child_population)), 
                                  "population_density"=c(mean(ratio.coefs$population_density), std_mean(ratio.coefs$population_density)))

data.frame("RMSE" = mean(ratio.mod_performance$RMSE), "R2" = mean(ratio.mod_performance$R2), "MAE" = mean(ratio.mod_performance$MAE))


## GAM ----
ratio.gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5) + s(elevation, k=5)  + s(poverty, k=5) +  s(distance_to_cities, k=5) +
                         s(Population, k=5) + s(child_population, k=5)  + s(population_density,k=5) )

# direction
ratio.gam.mod <- gam(ratio.gam.form, data = pentaTrain, method = "REML") 
summary(ratio.gam.mod)
par(mfrow=c(1,7))
plot(ratio.gam.mod)

# SE
set.seed(0)
ratio.gam.coefs <- data.frame("fertility"=NA,"elevation"=NA, "poverty"=NA, "distance_to_cities"=NA, "Population"=NA, "child_population"=NA, "population_density"=NA)
ratio.gam.mod_performance <- data.frame("RMSE" = rep(0, 100), "R2" = rep(0, 100), "MAE"=rep(0, 100))

for (i in 1:100) {
  sample_raw = pentaTrain.raw[sample(1:nrow(pentaTrain.raw), nrow(pentaTrain.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  pentaTest <- test_scale(pentaTest.raw,train.mean,train.sd)
  
  gam.mod <- gam(ratio.gam.form, data = sample_d, method = "REML") 
  
  raw_pred = predict(gam.mod,pentaTest)
  preds <- raw_pred*train.sd[8]+train.mean[8]
  rmse <- rmse(pentaTest.raw[,8],preds)
  r2 <- R2(pentaTest.raw[,8],preds)
  mae <- MAE(pentaTest.raw[8],preds)
  
  ## fill in the blank list
  ratio_gam_summary <- summary(gam.mod$finalModel)
  ratio_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
  ratio_gam_cfs  <- as.data.frame(t(ratio_gam_cfs))
  names(ratio_gam_cfs) <- c("fertility","elevation" ,"poverty","distance_to_cities", "Population","child_population","population_density")
  
  ratio.gam.coefs <- rbind(ratio.gam.coefs, ratio_gam_cfs)
  
  ratio.gam.mod_performance[i,1] <- rmse
  ratio.gam.mod_performance[i,2] <- r2
  ratio.gam.mod_performance[i,3] <- mae
  
  print(i)
}

ratio.gam.coefs <- ratio.gam.coefs[-1,]

ratio.gam.coef_clean <- ratio.gam.coefs[is.finite(rowSums(ratio.gam.coefs)),]
ratio.gam.coef_final <- data.frame( "fertility"=c(mean(ratio.gam.coef_clean$fertility),std_mean(ratio.gam.coef_clean$fertility)), 
                          "elevation"=c(mean(ratio.gam.coef_clean$elevation),std_mean(ratio.gam.coef_clean$elevation)), 
                          "poverty"=c(mean(ratio.gam.coef_clean$poverty), std_mean(ratio.gam.coef_clean$poverty)),
                          "distance_to_cities"=c(mean(ratio.gam.coef_clean$distance_to_cities),std_mean(ratio.gam.coef_clean$distance_to_cities)), 
                          "Population"=c(mean(ratio.gam.coef_clean$Population), std_mean(ratio.gam.coef_clean$Population)), 
                          "child_population"=c(mean(ratio.gam.coef_clean$child_population), std_mean(ratio.gam.coef_clean$child_population)),
                          "population_density"=c(mean(ratio.gam.coef_clean$population_density), std_mean(ratio.gam.coef_clean$population_density)))


ratio.gam.mod_clean <- ratio.gam.mod_performance[is.finite(rowSums(ratio.gam.coefs)),]
data.frame("RMSE" = mean(ratio.gam.mod_performance$RMSE), "R2" = mean(ratio.gam.mod_performance$R2), "MAE" = mean(ratio.gam.mod_performance$MAE))




## ridge ----
clinic_y <- ucs$OutreachProportion  # exclude outlier (coverage rate > 1)
clinic_x <- ucs[, c(1:7)]

par(mfrow = c(3,2))
for (i in 5:7) {
  plot(clinic_y ~ clinic_x[,i])
  title(main = colnames(clinic_x)[i])
  plot(clinic_y ~ log(clinic_x[,i]))
  title(main = paste0("y ~ log(",colnames(clinic_x)[i],")"))
}

### residual plots ----
# train set
log.train.raw <- pentaTrain.raw[, c(1:7)]
log.train.raw <- apply(log.train.raw, 2, log) %>%
  as.data.frame()
## na and inf produced, fill with mean
for (i in 1:ncol(log.train.raw)) {
  if (sum(is.na(log.train.raw[,i])) > 0) {
    log.train.raw[is.na(log.train.raw[,i]),i] <- mean(log.train.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.train.raw[,i])) > 0) {
    log.train.raw[is.infinite(log.train.raw[,i]),i] <- mean(log.train.raw[!is.infinite(log.train.raw[,i]),i],na.rm=TRUE)
  }
}
## add outcome col
log.train.raw <- cbind(log.train.raw, pentaTrain.raw[,8])
colnames(log.train.raw)[8] <- "OutreachProportion"

# find optimal lambda
lmod <- lm(OutreachProportion ~., data=log.train.raw)
vif(lmod)

y <- log.train.raw[, "OutreachProportion"]
X <- data.matrix(log.train.raw[, -8])

lambda <- c(0, 0.005, 0.01, 0.02, 0.04, 0.08, 1,5,10,20,22,23,24,25,30,40,50,100)
lridge <- ridge(y,X, lambda=lambda)
coef(lridge)

vridge <- vif(lridge)
vridge
## lambda = 20

# fit model
sample_d <- scale(log.train.raw) %>%
  as.data.frame()
y <- sample_d$OutreachProportion
x <- data.matrix(sample_d[, -8])

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 20,family = c("gaussian"), standardize = F)
ridge_outcome <- coef(ridge_best_model)

# residual plots
fitted <- predict(ridge_best_model, newx = x)
residual <- y - fitted
fitted <- fitted 
residual <- residual
par(mfrow=c(1,1))
plot(fitted, residual)

### fit model and estimate SE ----

# log transformation on test set
log.test.raw <- pentaTest.raw[, c(1:7)]
log.test.raw <- apply(log.test.raw, 2, log) %>%
  as.data.frame()
# na produced, fill with mean
for (i in 1:ncol(log.test.raw)) {
  if (sum(is.na(log.test.raw[,i])) > 0) {
    log.test.raw[is.na(log.test.raw[,i]),i] <- mean(log.test.raw[,i],na.rm=TRUE)
  }
  if (sum(is.infinite(log.test.raw[,i])) > 0) {
    log.test.raw[is.infinite(log.test.raw[,i]),i] <- mean(log.test.raw[!is.infinite(log.test.raw[,i]),i],na.rm=TRUE)
  }
}
# add outcome col
log.test.raw <- cbind(log.test.raw, pentaTest.raw[,9])
colnames(log.test.raw)[6] <- "OutreachProportion"

# fit
set.seed(0)
coefs <- data.frame("Intercept"= rep(0, 1000),"fertility"=rep(0, 1000),"elevation"=rep(0, 1000), "poverty"=rep(0, 1000),"distance_to_cities"=rep(0, 1000),
                    "Population"=rep(0, 1000),"child_population"=rep(0, 1000),"population_density"=rep(0, 1000))
mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_raw = log.train.raw[sample(1:nrow(log.train.raw), nrow(log.train.raw), replace = TRUE), ]
  train.sd <- apply(sample_raw, 2, sd)
  train.mean <- apply(sample_raw, 2, mean)
  sample_d <- scale(sample_raw) %>%
    as.data.frame()
  
  pentaTest <- test_scale(log.test.raw,train.mean,train.sd)
  
  y <- sample_d$OutreachProportion
  x <- data.matrix(sample_d[, -4])
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = 20,family = c("gaussian"), standardize = F)
  ridge_outcome <- coef(ridge_best_model)
  
  raw_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,-8]))
  preds <- raw_preds*train.sd[8]+train.mean[8]
  rmse <- rmse(pentaTest.raw[,8],preds)
  r2 <- R2(pentaTest.raw[,8],preds)
  mae <- MAE(pentaTest.raw[,8],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}


coef_final_log <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), 
                             "fertility"= c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                             "elevation"= c(mean(coefs$elevation), std_mean(coefs$elevation)), 
                             "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                             "distance_to_cities"= c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), 
                             "Population"=c(mean(coefs$Population), std_mean(coefs$Population)),
                             "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)),
                             "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)))

metrics.log <- data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))
metrics.log
View(t(coef_final_log))