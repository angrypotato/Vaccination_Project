### tehsil

### get_geovars function


tehsils_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils@data$id <- rownames(tehsils@data)
tehsils <- data.frame(tehsils)
tehsils <- tehsils[which(tehsils$PROVINCE == 'PUNJAB'),]
tehsils$TEHSIL <- sapply(tehsils$TEHSIL,solve_name)
tehsils <- tehsils[!(tehsils$TEHSIL %in% c('RAZMAK','FAISALABAD SADDAR')),]
tehsils[which(tehsils$TEHSIL == "SAHIWAL" & tehsils$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"
### 136 obs


# tehsils <- get_geovars("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif","fertility",1)

f <- "VaccinationStudy/Data/PAK_births_pp_v2_2015.tif"
attr <- "fertility"
res <- 1

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
    merged <- merge(tehsils, binded_df, by='Tehsil', all.x = T)  ###
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
