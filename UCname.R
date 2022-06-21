
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
