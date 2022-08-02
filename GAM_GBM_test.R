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





## GBM ----

### Producing the GBM Model with these significant features
### Tuned Learning Rate, Tree Complexity, K-Folds Validation

outreach.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:3,5:7,9,10,14),
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




## GAM ----

library(mgcv)

gam.form <- as.formula(TotalOutreachCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) + 
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + 
                         s(radio, k=5) + s(electricity, k=5) + s(antenatal_care, k=5) )

outreach_gam_model <- gam(gam.form, data = pentaTrain, method = "REML") 

outreach_gam_preds <- predict(outreach_gam_model,pentaTest)
outreach_gam_RMSE <- rmse(pentaTest[,20],outreach_gam_preds)
outreach_gam_R2 <- R2(pentaTest[,20],outreach_gam_preds)
outreach_gam_MAE <- MAE(pentaTest[,20],outreach_gam_preds)

outreach_gam_summary <- summary(outreach_gam_model)
outreach_gam_cfs <- -log10(as.data.frame(outreach_gam_summary$s.table)['p-value'])
xtable(data.frame(outreach_gam_cfs))

