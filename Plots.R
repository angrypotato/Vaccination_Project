# Correlation plots ----

library(corrplot)

## Tehsil level ----

tehsils <- read.csv("results/tehsils_complete.csv")

tehsils.plot <- tehsils[,c(14:26,28,30,32,34,38,39, 42:44)] %>%   # 19 covariates + 3 outcome variables
  as.data.frame() %>%
  dplyr::select(c(20,22,21,1,7:9,19,18,3,5,6,2,15,14,11,12,17,13,10,4,16)) %>%
  na.omit()

tehsils.cor <- cor(tehsils.plot, method = c("spearman"))

corrplot(tehsils.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)

### poverty
cor(tehsils$poverty, tehsils$TotalClinicsCoverage, method = "spearman")
cor(tehsils$poverty, tehsils$TotalOutreachCoverage, method = "spearman")
cor(tehsils$poverty, tehsils$OutreachProportion, method = "spearman", use = "complete.obs")



## UC level ----

ucs <- read.csv("results/ucs_complete.csv")

ucs.plot <- ucs[,c(34:36, 25:31)] %>%   # 7 features 
  as.data.frame() %>%
  na.omit() %>%
  dplyr::select(3,2,1,6,9,4,5,7,8,10)

ucs.cor <- cor(ucs.plot, method = c("spearman"))

corrplot(ucs.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)




# Maps ----



