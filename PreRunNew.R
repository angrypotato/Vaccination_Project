## Purpose: Source File For Necessary Libraries and Functions 

# Call Necessary Libraries ----

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

# Functions ----

## To Integrate Geographic Covariates With Either Tehsil or UC Level Data ----
## F = Covariate Source File
## attr = Covariate Column Name - IE Poverty or Fertility
## res = Indicate Whether Tehsil or District Level Data - res = 1 for Tehsil, 2 for District

get_geovars <- function(f,attr,res){
  file <- as.data.frame(raster(f),xy=TRUE)
  coordinates(file)<- ~x +y
  proj4string(file) <- proj4string(tehsils_shp)
  pts2 <- over(file,tehsils_shp)
  names(file)[1] <- "Attr"
  binded <- cbind(pts2,file$Attr)
  print(names(binded))
  binded_df <- data.frame("District" = binded[,3], "Tehsil" = binded[,4], "Population" = binded[,9])
  binded_df<- binded_df %>% 
    mutate(Tehsil = toupper(Tehsil))
  binded_df<- binded_df %>% 
    mutate(District = toupper(District))
  binded_df[which(binded_df$Tehsil == "SAHIWAL" & binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"
  if(res == 1){
    binded_df <- data.frame(binded_df[complete.cases(binded_df),] %>% group_by(Tehsil) %>% summarise_each(funs(nz_mean)))
    binded_df <- data.frame(binded_df %>% group_by(Tehsil) %>% summarise_each(funs(nz_mean)))
    tehsils$attr <- 0
    tehsils$Tehsil <- tehsils$TEHSIL
    merged <- merge(tehsils, binded_df, by='Tehsil')
  }
  else{
    binded_df <- data.frame(binded_df[complete.cases(binded_df),] %>% group_by(District) %>% summarise_each(funs(nz_mean)))
    binded_df <- data.frame(binded_df %>% group_by(District) %>% summarise_each(funs(nz_mean)))
    districts$attr <- 0
    districts$District <- districts$DISTRICT
    merged <- merge(districts, binded_df, by='District')
    
  }
  merged$attr <- merged[,ncol(merged)]
  merged <- merged[,-c(ncol(merged)-2,ncol(merged)-1)]
  names(merged)[length(names(merged))] <- attr
  merged
}



### test
## tehsils pre in AC
file <- as.data.frame(raster("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif"),xy=TRUE)
coordinates(file)<- ~x +y
proj4string(file) <- proj4string(tehsils_shp)
pts <- over(file,tehsils_shp)
names(file)[1] <- "Attr"
binded <- cbind(pts,file$Attr)
print(names(binded))
binded_df <- data.frame("District" = binded[,3], "Tehsil" = binded[,4], "Population" = binded[,9])
binded_df<- binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))
binded_df<- binded_df %>% 
  mutate(District = toupper(District))
binded_df[which(binded_df$Tehsil == "SAHIWAL" & binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

  binded_df <- data.frame(binded_df[complete.cases(binded_df),] %>% group_by(Tehsil) %>% summarise_each(funs(nz_mean)))
  binded_df <- data.frame(binded_df %>% group_by(Tehsil) %>% summarise_each(funs(nz_mean)))
  tehsils$fertility <- 0
  tehsils$Tehsil <- tehsils$TEHSIL
  merged <- merge(tehsils, binded_df, by='Tehsil')


merged$fertility <- merged[,ncol(merged)]
merged <- merged[,-c(ncol(merged)-2,ncol(merged)-1)]
names(merged)[length(names(merged))] <- "fertility"
tehsils <- merged
## works




### test2  UC level
file2 <- as.data.frame(raster("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif"),xy=TRUE)

coordinates(file2)<- ~x +y
proj4string(file2) <- proj4string(uc_shp)  ##
pts2 <- over(file2,uc_shp)    ##
names(file2)[1] <- "Attr"
binded2 <- cbind(pts2,file2$Attr)
print(names(binded2))
binded_df2 <- data.frame("District" = binded2[,2], "Tehsil" = binded2[,3], "UC" = binded2[,4], "Population" = binded2[,20])
binded_df2<- binded_df2 %>% 
  mutate(Tehsil = toupper(Tehsil))
binded_df2<- binded_df2 %>% 
  mutate(District = toupper(District))
binded_df2[which(binded_df2$Tehsil == "SAHIWAL" & binded_df2$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

  binded_df2 <- data.frame(binded_df2[complete.cases(binded_df2),] %>% group_by(UC) %>% summarise_each(funs(nz_mean)))
  binded_df2 <- data.frame(binded_df2 %>% group_by(UC) %>% summarise_each(funs(nz_mean)))
  ucs$attr <- 0
  ucs$UC <- ucs$DISTRICT
  merged <- merge(ucs, binded_df2, by='UC')
  
merged$fertility <- merged[,ncol(merged)]
merged <- merged[,-c(ncol(merged)-2,ncol(merged)-1)]
names(merged)[length(names(merged))] <- "fertility"
merged
length(unique(merged$UC))
### works for names matched


### solve uc name:

### data from shapefile
ucs <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs@data$id <- rownames(ucs@data)
ucs <- data.frame(ucs)
ucs <- ucs[which(ucs$PROVINCE == 'Punjab'),]
nrow(ucs)   ### 3445 (in paper)
attach(ucs)
ucs_names <- ucs[order(DISTRICT, TEHSIL, UC),]
length(unique(ucs_names$UC)) ### 2801


### data from the covariate file (e.g. fertility)
ucnames <- sort(unique(binded_df2$UC))
length(unique(ucnames)) ### 4974
length(unique(binded_df2$UC))   ### 4974
length(unique(binded_df2$Population))   ### 4919


### match (base on ucnames?)
setdiff(unique(ucs_names$UC), ucnames)   ### 35
setdiff(ucnames, unique(ucs_names$UC))   ### 1207 
which(ucs_names$UC == "100/wb (garh")
