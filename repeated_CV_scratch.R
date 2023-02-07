# repeated k-fold CV scratch
# clinic_gam

## partition data
k <- 5
set.seed(12345)

partition.df <- mutate(clinic.complete,
                   my.folds = sample(1:k, size = nrow(clinic.complete),replace = TRUE))

## train and evaluate model on each fold
cv.func.gam <- function(this.fold, data){
  
  train <- filter(data, my.folds != this.fold)
  validate <- filter(data, my.folds == this.fold)
  # scale
  train.sd <- apply(train, 2, sd)
  train.mean <- apply(train, 2, mean)
  train <- scale(train) %>%
    as.data.frame()
  validate <- test_scale(validate,train.mean,train.sd)
  
  # fit model
  model <- gam(gam.form, data = train, method = "REML")  
 
  # predict and evaluation
  raw_pred = predict(gam.mod,validate)
  preds <- raw_pred*train.sd[20]+train.mean[20]  # the col of outcome
  rmse <- rmse(pentaTest.raw[,20],preds)
  r2 <- R2(pentaTest.raw[,20],preds)
  mae <- MAE(pentaTest.raw[,20],preds)
  
  # storing the result 
  # to be edited
  
}

## repeating the k-fold CV
## rbind the results for each iteration

coefs <- data.frame("fertility"=NA, "elevation"=NA, "poverty"=NA, "night_lights"=NA, "Population"=NA,"child_population"=NA,"population_density"=NA, 
                    "radio"=NA,  "electricity"=NA, "television"=NA,"mobile_phone"=NA, "mothers_age"=NA)
mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

# for a single partition
# need to write the repeated part
# for loop might be better
cv.error <- sapply(seq_len(k),
                   FUN = cv.fun,
                   data = sim_data) %>%
  mean()


