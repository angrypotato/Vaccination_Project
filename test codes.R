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
tehsils_test <- data.frame(merged)[which(merged$PROVINCE == "PUNJAB"), ]
## works






### test2 UC level
file2 <- as.data.frame(raster("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif"),xy=TRUE)

coordinates(file2)<- ~x +y
proj4string(file2) <- proj4string(uc_shp)  ##
pts2 <- over(file2,uc_shp)    ##
names(file2)[1] <- "Attr"
binded2 <- cbind(pts2,file2$Attr)
print(names(binded2))
binded_df2 <- data.frame("District" = binded2[,2], "Tehsil" = binded2[,3], "UC" = binded2[,4], "Population" = binded2[,20])
binded_df2 <- binded_df2 %>% 
  mutate(Tehsil = toupper(Tehsil)) %>%
  mutate(District = toupper(District)) %>%
  mutate(UC = toupper(UC))

binded_df2 <- data.frame(binded_df2[complete.cases(binded_df2),] %>% group_by(UC) %>% summarise_each(funs(nz_mean)))
binded_df2 <- data.frame(binded_df2 %>% group_by(UC) %>% summarise_each(funs(nz_mean)))
ucs$attr <- 0
ucs$UC <- toupper(ucs$UC)
merged2 <- merge(ucs, binded_df2, by='UC')

merged2$fertility <- merged2[,ncol(merged2)]
merged2 <- merged2[,-c(ncol(merged2)-2,ncol(merged2)-1)]
names(merged2)[length(names(merged2))] <- "fertility"
uc_test <- merged2 %>%
  distinct()
nrow(uc_test)  ### 3248
length(unique(uc_test$UC))  ### 2767 != 3248, some ucs share the same name
### works for names matched




### solve uc name:

### method 1: remove rows with duplicate names
uc_duplicate <- data.frame(table(ucs$UC))
uc_duplicate <- uc_duplicate[uc_duplicate$Freq > 1, ]
uc_dup <- uc_duplicate$Var1
ucs_rmv_dup <- ucs[-which(ucs$UC %in% uc_dup),]   ### 2739 obs.
uc_test <- ucs_rmv_dup

### test method 1
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
  uc_test$attr <- 0
  uc_test$UC <- toupper(uc_test$UC)
  merged <- merge(uc_test, binded_df, by='UC')
  
  merged$attr <- merged[,ncol(merged)]
  merged <- merged[,-c(ncol(merged)-2,ncol(merged)-1)]
  names(merged)[length(names(merged))] <- attr
  merged
}

uc_test <- get_geovars_uc("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif","fertility")
uc_test <- get_geovars_uc("VaccinationStudy/Data/PAK_msk_alt/PAK_msk_alt.grd","elevation")
uc_test <- get_geovars_uc("VaccinationStudy/Data/pak07povmpi.tif","poverty")   
### got 2541 obs.
### method 1 seems to be working for the above features
# write.csv(uc_test, "D:\\Xiaoting\\Vaccination_Project\\results\\uc_covar_test.csv")




### other attempts
### data from shapefile
ucs <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs@data$id <- rownames(ucs@data)
ucs <- data.frame(ucs)
ucs <- ucs[which(ucs$PROVINCE == 'Punjab'),]
nrow(ucs)   ### 3445 (in paper)
attach(ucs)
ucs_names <- ucs[order(DISTRICT, TEHSIL, UC),]
length(unique(ucs_names$UC)) ### 2802


### data from the covariate file (e.g. fertility)
ucnames <- binded_df2[order(binded_df2$UC),]
length(unique(ucnames)) ### 4974
length(unique(binded_df2$UC))   ### 4974
length(unique(binded_df2$Population))   ### 4919


### match (base on ucnames?)
setdiff(unique(ucs_names$UC), ucnames)   ### 35
setdiff(ucnames, unique(ucs_names$UC))   ### 1207 
which(ucs_names$UC == "100/wb (garh")