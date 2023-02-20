
source(file='PreRunNew.r')

# Correlation plots ----


## Tehsil level ----

tehsils <- read.csv("results/tehsils_complete_9.15.csv")

tehsils.plot <- tehsils[-c(24,25,31,61,113), c(28,27,26,4:18,20,22,30,25)] %>%
  rename("Outreach Proportion" = OutreachProportion,
         "Clinic Vacc Covereage"= TotalClinicsCoverage, "Outreach Vacc Coverage" =TotalOutreachCoverage, "Fertility" = fertility, 
         "Child Population"=child_population, "Population Density"=population_density, 
         "Distance to Cities"=distance_to_cities, "Urban Vs Rural" =urban_to_rural, "Poverty"=poverty, "Distance to Lakes/Rivers" =distance_to_lakes_rivers, 
         "Elevation"=elevation, "Antenatal Care"=antenatal_care, "Vaccination Card"=card, "Electricity"=electricity, "Television"=television, "Maternal Education"=edu_mode,
         "Mobile Phone"=mobile_phone, "Radio"=radio, "Mother Age"=mothers_age, "# of Clinics"=fac_number, "Night Lights"=night_lights)

tehsils.plot <- tehsils.plot[complete.cases(tehsils.plot),]

### poverty
### controlling for other covariates
summary(lm(OutreachProportion ~ ., tehsils.plot[,-c(2,3)]))
summary(lm(TotalOutreachCoverage ~ ., tehsils.plot[,-c(1,2)]))
summary(lm(TotalClinicsCoverage ~ ., tehsils.plot[,-c(1,3)]))


tehsils.cor <- cor(tehsils.plot[complete.cases(tehsils.plot),], method = c("pearson"))


library(corrplot)
corrplot(tehsils.cor, tl.col = "black", tl.cex = 1.8, tl.srt = 45, cl.cex = 1.8)
## exported with width & height 2000



## UC level ----

ucs <- read.csv("results/uc_complete_buffer12.csv")

ucs.plot <- ucs[,-c(1:5,13,14)] %>%   # 7 features 
  as.data.frame() %>%
  na.omit() %>%
  dplyr::select(10:8,3,6,1,2,4)

### poverty 

### controlling for other covariates
summary(lm(OutreachProportion ~ ., ucs.plot[,-c(1,2)]))
summary(lm(TotalOutreachCoverage ~ ., ucs.plot[,-c(1,3)]))
summary(lm(TotalClinicsCoverage ~ ., ucs.plot[,-c(2,3)]))


ucs.plot <- ucs[,-c(1:5,13,14)] %>%   # 7 features 
  as.data.frame() %>%
  na.omit() %>%
  dplyr::select(10:8,3,6,1,4,2) %>%
  rename("Outreach Proportion" = OutreachProportion,"Clinic Vacc Covereage"= TotalClinicsCoverage, "Outreach Vacc Coverage" =TotalOutreachCoverage, 
         "Fertility" = fertility, "Child Population"=child_population, "Distance to Cities"=distance_to_cities, "Poverty"=poverty,"Elevation"=elevation)
  

ucs.cor <- cor(ucs.plot, method = c("pearson"))

corrplot(ucs.cor, tl.col = "black", tl.srt = 45)

corrplot(ucs.cor, tl.col = "black", tl.cex = 1, tl.srt = 45, cl.cex =1, cl.pos = 1)
## exported with width & height 2000



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

punjab.map <- merge(punjab.polygon, tehsils.map[,c(2,24:28)], by = "TEHSIL", all.x = T)

### fill NA with mean
## mutate(fac_number = replace_na(fac_number,as.integer(mean(fac_number, na.rm = TRUE))), clinic_per_child = replace_na(clinic_per_child,as.integer(mean(clinic_per_child, na.rm = TRUE))))
  
# class(punjab.map)

my_theme <- theme(legend.position = c(0.9, 0.2),
                  legend.title = element_text(colour="black", size=48, face="bold"),
                  legend.text=element_text(size=40),
                  legend.key.size = unit(3, 'cm'))

fac_num <- ggplot(punjab.map) + 
  geom_sf(aes(fill=fac_number)) +
  scale_fill_gradient(name = "Number of\nClinics", low="lightgreen", high="darkgreen") +
  my_theme 

in_clinic <- ggplot(punjab.map) + 
  geom_sf(aes(fill=TotalClinicsCoverage)) +
  scale_fill_gradient(name = "In-clinic vacc per\nchild capita", low="lightgreen", high="darkgreen", breaks = seq(0,0.2,0.05)) +
  my_theme 

outreach <- ggplot(punjab.map) + 
  geom_sf(aes(fill=TotalOutreachCoverage)) +
  scale_fill_gradient(name = "Outreach vacc per\nchild capita", low="lightgreen", high="darkgreen", breaks = seq(0.1,0.7,0.1)) +
  my_theme 

proportion <- ggplot(punjab.map) + 
  geom_sf(aes(fill=OutreachProportion)) +
  scale_fill_gradient(name = "Outreach\nProportion", low="lightgreen", high="darkgreen") +
  my_theme

## 2000


### map on the upper left
pak <- getData("GADM", country="PK", level=1)
pak.province <- fortify(pak, region = "NAME_1") %>%
  mutate(punjab = 0)
pak.province[which(pak.province$id == "Punjab"),]$punjab <- 1
theme_set(theme_void())
pak <- ggplot(pak.province, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(color = as.factor(punjab), fill = as.factor(punjab)), size = 1.5) +
  scale_color_manual(values = c('1' = 'red', '0' = "Black")) +
  scale_fill_manual(values =  c('1' = 'white', '0' = "white")) +
  theme(legend.position = "none")
##2000


### scatter plot
# tehsils <- read.csv("results/tehsils_complete_9.15.csv")
tehsils.scatter <- tehsils[order(tehsils$OutreachProportion, decreasing = T),] %>%
  mutate(Tehsil = as.numeric(1:137))

theme_set(theme_classic())
e <- ggplot(tehsils.scatter, aes(x=Tehsil, y= OutreachProportion)) +
  geom_point(size=7, shape=1) +
  scale_x_continuous(breaks = seq(from = 0, to = 140, by = 20)) +
  scale_y_continuous(breaks = seq(from = 0.5, to = 1, by = 0.1)) +
  xlab("Tehsil (index)") + 
  ylab("Outreach/all vaccination ratio") +
  theme(axis.title = element_text(size = 45,color = "black", face="bold"),
        axis.text = element_text(size =40))
## 1500


 ## combine plots ---- 

library(ggpubr)
theme_set(theme_void())

figure <- ggarrange(fac_num, clinics, outreach, proportion, e,
                    labels = c("A", "B", "C", "D", "E"), label.y = 0.8,
                    ncol = 5, nrow = 1, align = "h")
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





