# Correlation plots ----

library(corrplot)

## Tehsil level ----

tehsils <- read.csv("results/tehsils_complete_new.csv")

tehsils.plot <- tehsils[-c(24,25,60,112),-c(1:5,21)] %>%  
  as.data.frame() %>%
  dplyr::select(c(21,23,22,1,8:10,7,20,3,5,2,16,15,12,13,19,14,11,18,24))   ### 132 obs.

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




# partial correlation matrix ----


#### 
library(ppcor)

PCOR <- function(x, type = c("raw", "cor")) {
  
  type <- match.arg(type)
  if (type == "raw") {
    x <- scale(x)
    R <- (t(x) %*% x) / (nrow(x) - 1)
  } else  {
    R <- x
  }
  ind <- unique(dim(R))
  R_inv <- ginv(R)
  ZM <- matrix(rep(0, len = (ind*ind)), nrow = ind)
  diag(ZM) <- diag(R_inv)
  D <- ginv(ZM)
  AICOV <- D %*% R_inv %*% D
  diag(ZM) <- diag(AICOV)
  D  <- ginv(sqrt(ZM))
  AICOR <- D %*% AICOV %*% D
  pcor <- AICOR
  pcor[upper.tri(pcor)] <- -pcor[upper.tri(pcor)]
  pcor[lower.tri(pcor)] <- -pcor[lower.tri(pcor)]
  dimnames(pcor) <- list(colnames(R), colnames(R))
  return(pcor)
  
}  

pcors <- PCOR(tehsils.plot, type = "raw")

pcors

corrplot(pcors, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)

# outreach proportion
pcors1 <- PCOR(tehsils.plot[,-c(2,3)], type = "raw")
corrplot(pcors1, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)


####
library(psych)
pcor <- partial.r(tehsils.plot, c(1:20), c(21:23), use="pairwise",method="pearson")
corrplot(pcors)





