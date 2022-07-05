# Correlation plots ----

library(corrplot)

## Tehsil level ----

tehsils <- read.csv("results/tehsils_complete.csv")

tehsils.plot <- tehsils[,c(13:25,27,29,31,33,37,38,41:43)] %>%   # 19 features
  as.data.frame() %>%
  dplyr::select(c(20:22, 1:19)) %>%
  na.omit()

tehsils.cor <- cor(tehsils.plot, method = c("spearman"))

corrplot(tehsils.cor, tl.col = "black", tl.cex = 0.9, tl.srt = 45)



## UC level ----

ucs <- read.csv("results/ucs_complete.csv")

ucs.plot <- ucs[,c(34:36, 25:31)] %>%   # 7 features 
  as.data.frame() %>%
  na.omit()

ucs.cor <- cor(ucs.plot, method = c("spearman"))

corrplot(ucs.cor, tl.col = "black", tl.cex = 0.9, tl.srt = 45)

# Maps ----

