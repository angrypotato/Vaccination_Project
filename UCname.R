
# load packages ----

library('rgdal')
library('ggplot2')
library('dplyr')
library('rgeos')
library('formattable')
library('tidyr')
library('foreign')
library('haven')
library('caret')
library('gbm')
library('randomForest')
library('xtable')
#library('malariaAtlas')
library('devtools')
library('gbm')
library('randomForest')
library('lars')
library('mlbench')
library('caret')
library('Metrics')
#library('gam')
library('htmltools')
library('caTools')
library('sp')
library('dismo')


# functions used ----

get_geovars_uc <- function(f,attr){
  file <- as.data.frame(raster(f),xy=TRUE)
  coordinates(file)<- ~x +y
  proj4string(file) <- proj4string(uc_shp)  
  pts <- over(file,uc_shp)    
  names(file)[1] <- "Attr"
  binded <- cbind(pts,file$Attr)
  print(names(binded))
  binded_df <- data.frame("District" = binded[,2], "Tehsil" = binded[,3], "UC" = binded[,4], "Population" = binded[,20])
  binded_df <- binded_df %>% 
    mutate(Tehsil = toupper(Tehsil)) %>%
    mutate(District = toupper(District)) %>%
    mutate(UC = toupper(UC))
  
  binded_df <- data.frame(binded_df[complete.cases(binded_df),] %>% group_by(UC) %>% summarise_each(funs(nz_mean)))
  binded_d2 <- data.frame(binded_df %>% group_by(UC) %>% summarise_each(funs(nz_mean)))
  ucs$attr <- 0
  ucs$UC <- toupper(ucs$UC)
  merged <- merge(ucs, binded_df, by='UC')
  
  merged$attr <- merged[,ncol(merged)]
  merged <- merged[ ,-c(ncol(merged)-2,ncol(merged)-1)]
  names(merged)[length(names(merged))] <- attr
  merged
}

solve_uc_name <- function(ucs) {
  name_list <- data.frame(table(ucs$UC))
  name_duplicate <- name_list[name_list$Freq > 1, ]
  name_dup_list <- name_duplicate$Var1
  uc_rmv_dup <- ucs[-which(ucs$UC %in% name_dup_list),] 
  uc_rmv_dup
}

nz_mean <- function(x){
  mean(x[x!=0])
}


# read in shape file ----

uc_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs@data$id <- rownames(ucs@data)
ucs <- data.frame(ucs)
ucs <- ucs[which(ucs$PROVINCE == 'Punjab'),]

## ucs <- solve_uc_name(ucs)  
## run this code to get result without duplicate names, otherwise with obs. having duplicate names


# Join Fertility, Elevation, Poverty and Night Lights Covariates ----

ucs <- get_geovars_uc("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif","fertility")
ucs <- get_geovars_uc("VaccinationStudy/Data/PAK_msk_alt/PAK_msk_alt.grd","elevation")
ucs <- get_geovars_uc("VaccinationStudy/Data/pak07povmpi.tif","poverty")
ucs <- get_geovars_uc("VaccinationStudy/Data/NLDI_2006_0p25_rev20111230.tif","night_lights")




##### recruit duplicate obs.

uc_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs@data$id <- rownames(ucs@data)
ucs <- data.frame(ucs)
ucs <- ucs[which(ucs$PROVINCE == 'Punjab'),] %>%
  mutate(UC = toupper(UC))


uc_dup <- data.frame(table(ucs$UC)) %>%
  filter(Freq > 1)
uc_dup_name <- uc_dup[[,1]]


ucs <- ucs[!duplicated(ucs$UC),]   ###

ucs <- get_geovars_uc("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif","fertility")
ucs <- get_geovars_uc("VaccinationStudy/Data/PAK_msk_alt/PAK_msk_alt.grd","elevation")
ucs <- get_geovars_uc("VaccinationStudy/Data/pak07povmpi.tif","poverty")




solve_uc_name <- function(ucs) {
  name_list <- data.frame(table(ucs$UC))
  name_duplicate <- name_list[name_list$Freq > 1, ]
  name_dup_list <- name_duplicate$Var1
  
  ucs$mark <- 1
  
  for (n in 1:nrow(ucs)) {
    obs_name <- ucs$UC[n]
    if (obs_name %in% name_dup_list) {
      
    }
  }
  
  
  ucs_out
}

### new uc_covar
uc_covar <- read.csv("results/ucs_covariates_7.8.csv")
test1 <- merge(uc_covar, uc_vacc, by = c("UC", "DISTRICT","TEHSIL"), all.x = T) %>%
       mutate(OutreachProportion = penta3_out_clinic / (penta3_in_clinic+penta3_out_clinic),
                           TotalOutreachCoverage = penta3_out_clinic / child_population,  
                           TotalClinicsCoverage = penta3_in_clinic / child_population) %>%
  filter(!(is.na(penta3_in_clinic) & is.na(penta3_out_clinic) & is.na(OutreachProportion)))  ### got 2541


### old uc_covar
test2 <- merge(uc_covar_old, uc_vacc, by = c("UC", "DISTRICT","TEHSIL"), all.x = T) %>%
  mutate(OutreachProportion = penta3_out_clinic / (penta3_in_clinic+penta3_out_clinic),
         TotalOutreachCoverage = penta3_out_clinic / child_population,  
         TotalClinicsCoverage = penta3_in_clinic / child_population) %>%
  filter(!(is.na(penta3_in_clinic) & is.na(penta3_out_clinic) & is.na(OutreachProportion)))  ### got 2541


### new uc_vacc
### nw uc_covar
uc_vacc <- read.csv("results/ucs_vacc_new.csv")
test3 <- merge(uc_covar, uc_vacc, by = c("UC", "DISTRICT","TEHSIL"), all.x = T) %>%
  mutate(OutreachProportion = penta3_out_clinic / (penta3_in_clinic+penta3_out_clinic),
         TotalOutreachCoverage = penta3_out_clinic / child_population,  
         TotalClinicsCoverage = penta3_in_clinic / child_population) %>%
  filter(!(is.na(penta3_in_clinic) & is.na(penta3_out_clinic) & is.na(OutreachProportion)))  ### got 3154

test3_na <- test3[is.nan(test3$OutreachProportion),]   ### got 19

test4 <- test3[!is.nan(test3$OutreachProportion),]

write.csv(test4, "results/uc_complete_test.csv")


### 7/11
uc_complete <- read.csv("results/uc_complete_test.csv")
uc_covar <- read.csv("results/ucs_covariates_7.8.csv")
uc_vacc <- read.csv("results/ucs_vacc_new.csv")

uc_complete_clean <- uc_complete[,c(2:4, 24:30, 51:53)]
uc_complete_clean <- uc_complete_clean[complete.cases(uc_complete_clean),] %>%
  distinct()   ### 2742

dup_uc <- data.frame(table(uc_complete_clean$UC)) %>% 
  filter(Freq > 1)   ### 0 dup uc names

write.csv(uc_complete_clean, "results/uc_complete_clean.csv")
