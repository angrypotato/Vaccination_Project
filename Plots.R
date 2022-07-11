# Correlation plots ----

library(corrplot)

## Tehsil level ----

tehsils <- read.csv("results/tehsils_complete_new.csv")

tehsils.plot <- tehsils[-c(29,69,104,116),-c(1:4,20)] %>%  
  as.data.frame() %>%
  dplyr::select(c(21,23,22,1,8:10,7,20,3,5,2,16,15,12,13,19,14,11,17))   ### 132 obs.

tehsils.cor <- cor(tehsils.plot, method = c("spearman"))

corrplot(tehsils.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)

### poverty
cor(tehsils$poverty, tehsils$TotalClinicsCoverage, method = "spearman")
cor(tehsils$poverty, tehsils$TotalOutreachCoverage, method = "spearman")
cor(tehsils$poverty, tehsils$OutreachProportion, method = "spearman", use = "complete.obs")



## UC level ----

ucs <- read.csv("results/uc_complete_clean.csv")

ucs.plot <- ucs[,-c(1:4)] %>%   # 7 features 
  as.data.frame() %>%
  na.omit() %>%
  dplyr::select(10:8,3,6,1,2,4)

ucs.cor <- cor(ucs.plot, method = c("spearman"))

corrplot(ucs.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)




# Maps ----



