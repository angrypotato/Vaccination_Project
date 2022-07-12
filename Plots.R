# Correlation plots ----

library(corrplot)

## Tehsil level ----

tehsils <- read.csv("results/tehsils_complete_new.csv")

tehsils.plot <- tehsils[-c(29,69,104,116),-c(1:4,20)] %>%  
  as.data.frame() %>%
  dplyr::select(c(21,23,22,1,8:10,7,20,3,5,2,16,15,12,13,19,14,11,17))   ### 132 obs.

tehsils.cor <- cor(tehsils.plot, method = c("pearson"))

corrplot(tehsils.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)

### poverty

library("Hmisc")

### using all 136 obs
rcorr(tehsils$poverty, tehsils$OutreachProportion, type = c("pearson"))
rcorr(tehsils$poverty, tehsils$TotalClinicsCoverage, type = c("pearson"))
rcorr(tehsils$poverty, tehsils$TotalOutreachCoverage, type = c("pearson"))

### using 132 obs
rcorr(tehsils.plot$poverty, tehsils.plot$OutreachProportion, type = c("pearson"))
rcorr(tehsils.plot$poverty, tehsils.plot$TotalClinicsCoverage, type = c("pearson"))
rcorr(tehsils.plot$poverty, tehsils.plot$TotalOutreachCoverage, type = c("pearson"))




## UC level ----

ucs <- read.csv("results/uc_complete_clean.csv")

ucs.plot <- ucs[,-c(1:4)] %>%   # 7 features 
  as.data.frame() %>%
  na.omit() %>%
  dplyr::select(10:8,3,6,1,2,4)

ucs.cor <- cor(ucs.plot, method = c("pearson"))

corrplot(ucs.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)

rcorr(ucs$poverty, ucs$OutreachProportion, type = c("pearson"))
rcorr(ucs$poverty, ucs$TotalClinicsCoverage, type = c("pearson"))
rcorr(ucs$poverty, ucs$TotalOutreachCoverage, type = c("pearson"))



# Maps ----



