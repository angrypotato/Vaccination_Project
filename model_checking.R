##### Model checking for GAM and Ridge models using in_clinic, outreach, and ratio data

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
tehsils <- read.csv("results/tehsils_complete_buffer12_9.27.csv")

tehsils.clinic <- tehsils[,c(3:17,19,21:23,28)]
tehsils.clinic <- tehsils.clinic[complete.cases(tehsils.clinic), ]  
set.seed(0)
clinic_data_split = sample.split(tehsils.clinic, SplitRatio = 0.8)
clinic.pentaTrain <- subset(tehsils.clinic, clinic_data_split == TRUE) %>%
  scale() %>%
  as.data.frame()

tehsils.outreach <- tehsils[,c(3:17,19,21:23,27)]
tehsils.outreach <- tehsils.outreach[complete.cases(tehsils.outreach),]
set.seed(43)
outreach_data_split = sample.split(tehsils.outreach, SplitRatio = 0.8)
outreach.pentaTrain <- subset(tehsils.outreach, outreach_data_split == TRUE) %>%
  scale() %>%
  as.data.frame()

tehsils.ratio <- tehsils[,c(3:17,19,21:23,26)] 
tehsils.ratio <- tehsils.ratio[complete.cases(tehsils.ratio), ]
set.seed(1)
ratio_data_split = sample.split(tehsils.ratio, SplitRatio = 0.8)
ratio.pentaTrain <- subset(tehsils.ratio, ratio_data_split == TRUE) %>%
  scale() %>%
  as.data.frame()


# Model checking ----

## GAM ----
## check df and homoscedasticity

### in clinic ----
clinic.gam.form <- as.formula(TotalClinicsCoverage ~ s(fertility, k=5) + s(elevation, k=5) + s(poverty, k=5) +  s(night_lights, k=5) +
                         s(distance_to_cities, k=5) +s(Population, k=5) + s(child_population, k=5) +  s(population_density, k=5) +
                         s(radio, k=5)+ s(electricity, k=5)+ s(television, k=5)  + s(mothers_age, k=5))

set.seed(0)

# using the whole train set
clinic_gam_model <- gam(clinic.gam.form, data = clinic.pentaTrain, method = "REML") 
gam.check(clinic_gam_model, k.rep = 500) 

# using bootstrapped data, get low p-value
for (i in 1:5) {
  sample_d = clinic.pentaTrain[sample(1:nrow(clinic.pentaTrain), nrow(clinic.pentaTrain), replace = TRUE), ]

  clinic_gam_model <- gam(clinic.gam.form, data = sample_d, method = "REML") 
  
  gam.check(clinic_gam_model, k.rep = 500) # how to save the plots?

  print(i)
}


### outreach ----
outreach.gam.form <- as.formula(TotalOutreachCoverage ~ s(night_lights, k=5) + 
                         s(distance_to_cities, k=5) + s(Population, k=5) + s(child_population, k=5) + s(electricity, k=5)+ 
                         s(antenatal_care, k=5) + s(mothers_age, k=5))

outreach_gam_model <- gam(outreach.gam.form, data = outreach.pentaTrain, method = "REML") 
gam.check(outreach_gam_model, k.rep = 500) 

# using bootstrapped data
set.seed(0)
for (i in 1:5) {
  sample_d = outreach.pentaTrain[sample(1:nrow(outreach.pentaTrain), nrow(outreach.pentaTrain), replace = TRUE), ]
  
  outreach_gam_model <- gam(outreach.gam.form, data = sample_d, method = "REML") 
  
  gam.check(outreach_gam_model, k.rep = 500) # how to save the plots?
  
  print(i)
}


### ratio ----
ratio.gam.form <- as.formula(OutreachProportion ~ s(fertility, k=5)   + s(poverty, k=5)  + 
                               s(population_density,k=5) +s(radio, k=5) + s(electricity, k=5)  + 
                               s(antenatal_care, k=5) + s(mothers_age, k=5) )

ratio_gam_model <- gam(ratio.gam.form, data = ratio.pentaTrain, method = "REML") 
gam.check(ratio_gam_model, k.rep = 500) 

# using bootstrapped data
set.seed(0)
for (i in 1:5) {
  sample_d = ratio.pentaTrain[sample(1:nrow(ratio.pentaTrain), nrow(ratio.pentaTrain), replace = TRUE), ]
  
  ratio_gam_model <- gam(ratio.gam.form, data = sample_d, method = "REML") 
  
  gam.check(ratio_gam_model, k.rep = 500) # how to save the plots?
  
  print(i)
}


## Rigde ----

### in clinic ----

### outreach ----

### ratio ----