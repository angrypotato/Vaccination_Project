
source(file='PreRunNew.r')

# Correlation plots ----

library(corrplot)

## Tehsil level ----

tehsils <- read.csv("results/tehsils_complete_7.18.csv")

tehsils.plot <- tehsils[ , -c(1,21,22)] %>%
  dplyr::select(c(21,23,22,1,7:9,6,8,19,3,5,2,15,14,11,12,18,13,10,17,20)) 

tehsils.cor <- cor(tehsils.plot[-c(24,25,31,61,113),], method = c("pearson"))

corrplot(tehsils.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)


### poverty ----

### controlling for other covariates
summary(lm(OutreachProportion ~ ., tehsils.plot[,-c(2,3)]))
summary(lm(TotalOutreachCoverage ~ ., tehsils.plot[,-c(1,2)]))
summary(lm(TotalClinicsCoverage ~ ., tehsils.plot[,-c(1,3)]))



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


## prep ----

tehsils.map <- tehsils %>%
  mutate(clinic_per_child = fac_number / child_population)

theme_set(theme_void())
library("sf")


## single plots ----
punjab.polygon <- st_read("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp") %>%
  filter(PROVINCE == "PUNJAB") %>%
  mutate(TEHSIL = sapply(TEHSIL,solve_name)) 

punjab.polygon[which(punjab.polygon$TEHSIL == "SAHIWAL" & punjab.polygon$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"

punjab.map <- merge(punjab.polygon, tehsils.map[,c(1,23:27)], by = "TEHSIL", all.x = T) %>%
  mutate(fac_number = replace_na(fac_number,as.integer(mean(fac_number, na.rm = TRUE))),   ### fill NA with mean
         clinic_per_child = replace_na(clinic_per_child,as.integer(mean(clinic_per_child, na.rm = TRUE))))
  
# class(punjab.map)



fac_num <- ggplot(punjab.map) + 
  geom_sf(aes(fill=fac_number)) +
  scale_fill_gradient(name = "Number of\nClinics", low="lightgreen", high="darkgreen") +
  theme(legend.position = c(0.9, 0.2),
        legend.title = element_text(colour="black", size=10, face="bold"))

clinics <- ggplot(punjab.map) + 
  geom_sf(aes(fill=clinic_per_child)) +
  scale_fill_gradient(name = "Clinic per\nchild capita", low="lightgreen", high="darkgreen") +
  theme(legend.position = c(0.9, 0.2),
        legend.title = element_text(colour="black", size=10, face="bold"))

outreach <- ggplot(punjab.map) + 
  geom_sf(aes(fill=TotalOutreachCoverage)) +
  scale_fill_gradient(name = "Outreach vacc/\nchild capita", low="lightgreen", high="darkgreen") +
  theme(legend.position = c(0.9, 0.2),
        legend.title = element_text(colour="black", size=10, face="bold"))

proportion <- ggplot(punjab.map) + 
  geom_sf(aes(fill=OutreachProportion)) +
  scale_fill_gradient(name = "Outreach\nProportion", low="lightgreen", high="darkgreen") +
  theme(legend.position = c(0.9, 0.2),
        legend.title = element_text(colour="black", size=10, face="bold"))



## combine plots ---- 

library(ggpubr)

figure <- ggarrange(fac_num, clinics, outreach, proportion,
                    labels = c("A", "B", "C", "D"), label.y = 0.8,
                    ncol = 4, nrow = 1)
figure

ggexport(figure, filename = "figure1.pdf")





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





