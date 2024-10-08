



## Purpose: To Extract Relevant Geographic and Demographic Covariates and Map Them To Tehsil and District Level Geographic Data 

# Call Source File for Required Libraries and Functions

source(file='PreRun.r')

# Intake DSS Data

dss_data <- read.csv(file = 'VaccinationStudy/Data/DSSData_3Years.csv')

# Filter Data to only Children < 5 

dss_data <- dss_data[(dss_data$Age < 5),]

# Clean and Convert Tehsil (Town), Districts and UC Names As to Prepare Them for Joining With Geographic Data

#Sys.setlocale(category="LC_ALL", locale = "English_United States.1252")

colnames(dss_data)[colnames(dss_data)=="Town.Tehsil"] <- "TEHSIL"
dss_data$TEHSIL <- sapply(toupper(dss_data$TEHSIL),solve_name)
dss_data$DISTRICT <- sapply(toupper(dss_data$Patient.s.District),solve_district_name)
dss_data$UC <- toupper(dss_data$UC)

# Get Tehsil (Town)  Geographic Data From Shapefile

tehsils_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils@data$id <- rownames(tehsils@data)

# Get District Geographic Data From Shapefile

districts_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/District_Boundary.shp")
districts <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/District_Boundary.shp")
districts@data$id <- rownames(districts@data)

# Get UC Geographic Data From Shapefile

uc_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs@data$id <- rownames(ucs@data)

# Convert Geographic Data to Data Frames

tehsils <- data.frame(tehsils)
districts <- data.frame(districts)

# Clean Geographic Field Names

tehsils$TEHSIL <- sapply(tehsils$TEHSIL,solve_name)
districts$DISTRICT <- sapply(districts$DISTRICT,solve_district_name)

# Isolate Punjab Data

districts <- districts[which(districts$PROVINCE == 'PUNJAB'),]
tehsils <- tehsils[which(tehsils$PROVINCE == 'PUNJAB'),]

# Remove Tehsils that were Mistakenly Labelled as being in Punjab

tehsils <- tehsils[!(tehsils$TEHSIL %in% c('RAZMAK','FAISALABAD SADDAR')),]

# Sahiwal is the name of 2 tehsils, one in Sahiwal district and another outside, therefore lets name the sahiwal in the sahiwal district - SAHIWAL_SAHIWAL

tehsils[which(tehsils$TEHSIL == "SAHIWAL" & tehsils$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"

# Join Fertility, Elevation, Poverty and Night Lights Covariates to Districts and Tehsils Using Function in Source File

tehsils <- get_geovars("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif","fertility",1)
districts <- get_geovars("VaccinationStudy/Data/PAK_births_pp_v2_2015.tif","fertility",2)
tehsils <- get_geovars("VaccinationStudy/Data/PAK_msk_alt/PAK_msk_alt.grd","elevation",1)
districts <- get_geovars("VaccinationStudy/Data/PAK_msk_alt/PAK_msk_alt.grd","elevation",2)
tehsils <- get_geovars("VaccinationStudy/Data/pak07povmpi.tif","poverty",1)
districts <- get_geovars("VaccinationStudy/Data/pak07povmpi.tif","poverty",2)
tehsils <- get_geovars("VaccinationStudy/Data/NLDI_2006_0p25_rev20111230.tif","night_lights",1)
districts <- get_geovars("VaccinationStudy/Data/NLDI_2006_0p25_rev20111230.tif","night_lights",2)

### Extract Distance to Lakes/Rivers Covariate and map them to the Tehsils Data

river<-readOGR("VaccinationStudy/Data/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
river@data$id <- rownames(river@data)
river_lines.df <- fortify(river)

coordinates(river_lines.df)<- ~long +lat
proj4string(river_lines.df) <- proj4string(tehsils_shp)

river_pts <- over(river_lines.df, tehsils_shp)
river_lines.df$number <- 1
river_binded <- cbind(river_pts, river_lines.df$number)
river_binded_df <- data.frame("District" = river_binded[,3],"Tehsil" = river_binded[,4], "Population" = river_binded[,9])
river_binded_df<- river_binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))
river_binded_df[which(river_binded_df$Tehsil == "SAHIWAL" & river_binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

river_poptable <- table(river_binded_df$Tehsil)
river_pop_df <- data.frame("Tehsil" = names(river_poptable))
river_pop_df$rivers <- 0

for(i in 1:length(river_poptable)){
  river_name <- names(river_poptable)[i]
  river_cells <-river_binded_df[which(river_binded_df$Tehsil == river_name),]
  river_pop<- sum(river_cells$Population,na.rm=TRUE)
  river_pop_df[which(river_pop_df$Tehsil == river_name),]$rivers <- river_pop
}
river_pop_dfs <- river_pop_df[which(river_pop_df$Tehsil %in% tehsils$TEHSIL),]

tehsils$distance_to_lakes_rivers <- 0
for(i in 1:NROW(river_pop_dfs)){
  river_cell <- river_pop_dfs[i,]
  river_cell$Tehsil <- sapply(river_cell$Tehsil,solve_name)
  if(NROW(river_cell)>0){
    tehsils[which(tehsils$TEHSIL == toupper(river_cell$Tehsil)),]$distance_to_lakes_rivers <- river_cell$rivers
  }
  
}

### Extract Distance to Lakes/Rivers Covariate and map them to the Districts Data


river<-readOGR("VaccinationStudy/Data/ne_10m_rivers_lake_centerlines/ne_10m_rivers_lake_centerlines.shp")
river@data$id <- rownames(river@data)
river_lines.df <- fortify(river)

coordinates(river_lines.df)<- ~long +lat
proj4string(river_lines.df) <- proj4string(districts_shp)

river_pts <- over(river_lines.df, districts_shp)
river_lines.df$number <- 1
river_binded <- cbind(river_pts, river_lines.df$number)
river_binded_df <- data.frame("District" = river_binded[,3], "Population" = river_binded[,9])
river_binded_df<- river_binded_df %>% 
  mutate(District = toupper(District))

river_poptable <- table(river_binded_df$District)
river_pop_df <- data.frame("District" = names(river_poptable))
river_pop_df$rivers <- 0

for(i in 1:length(river_poptable)){
  river_name <- names(river_poptable)[i]
  river_cells <-river_binded_df[which(river_binded_df$District == river_name),]
  river_pop<- sum(river_cells$Population,na.rm=TRUE)
  river_pop_df[which(river_pop_df$District == river_name),]$rivers <- river_pop
}
river_pop_dfs <- river_pop_df[which(river_pop_df$District %in% districts$DISTRICT),]

districts$distance_to_lakes_rivers <- 0
for(i in 1:NROW(river_pop_dfs)){
  river_cell <- river_pop_dfs[i,]
  river_cell$District <- sapply(river_cell$District,solve_district_name)
  if(NROW(river_cell)>0){
    districts[which(districts$DISTRICT == toupper(river_cell$District)),]$distance_to_lakes_rivers <- river_cell$rivers
  }
  
}

### Extract Malaria Instance Covariate and map them to the Tehsils Data
# (NOTE THAT THIS DATA IS FROM A SOURCE (MALARIA ATLAS) THAT IS NO LONGER VIABLE - WILL NOT RUN)

MDG_shp <- getShp(ISO = "PAK", admin_level = "admin0")
MDG_shp@data$id <- rownames(MDG_shp@data)
MDG_shp.df <- fortify(MDG_shp)
coordinates(MDG_shp.df)<- ~long +lat
proj4string(MDG_shp.df) <- proj4string(tehsils_shp)


mal_pts <- over(MDG_shp.df, tehsils_shp)
MDG_shp.df$number <- 1
mal_binded <- cbind(mal_pts, MDG_shp.df$number)
mal_binded_df <- data.frame("District" = mal_binded[,3],"Tehsil" = mal_binded[,4], "Population" = mal_binded[,9])
mal_binded_df<- mal_binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))
# mal_binded_df[which(mal_binded_df$Tehsil == "SAHIWAL" & mal_binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"
# which(mal_binded_df$Tehsil == "SAHIWAL" & mal_binded_df$District == "SAHIWAL")   outputs: integer(0)

mal_poptable <- table(mal_binded_df$Tehsil)
mal_pop_df <- data.frame("Tehsil" = names(mal_poptable))
mal_pop_df$malaria_instance <- 0

for(i in 1:length(mal_poptable)){
  mal_name <- names(mal_poptable)[i]
  mal_cells <-mal_binded_df[which(mal_binded_df$Tehsil == mal_name),]
  mal_pop<- sum(mal_cells$Population,na.rm=TRUE)
  mal_pop_df[which(mal_pop_df$Tehsil == mal_name),]$malaria_instance <- mal_pop
}
mal_pop_dfs <- mal_pop_df[which(mal_pop_df$Tehsil %in% tehsils$TEHSIL),]

tehsils$malaria_instance <- 0
for(i in 1:NROW(mal_pop_dfs)){
  mal_cell <- mal_pop_dfs[i,]
  mal_cell$Tehsil <- sapply(mal_cell$Tehsil,solve_name)
  if(NROW(tehsils[which(tehsils$TEHSIL == toupper(mal_cell$Tehsil)),]$births) > 0){      
    ### tehsils$births???
    tehsils[which(tehsils$TEHSIL == toupper(mal_cell$Tehsil)),]$malaria_instance <- mal_cell$malaria_instance
  }
}

### Extract Malaria Instance Covariate and map them to the Districts Data
# (NOTE THAT THIS DATA IS FROM A SOURCE (MALARIA ATLAS) THAT IS NO LONGER VIABLE - WILL NOT RUN)


MDG_shp <- getShp(ISO = "PAK", admin_level = "admin0")
MDG_shp@data$id <- rownames(MDG_shp@data)
MDG_shp.df <- fortify(MDG_shp)
coordinates(MDG_shp.df)<- ~long +lat
proj4string(MDG_shp.df) <- proj4string(districts_shp)


mal_pts <- over(MDG_shp.df, districts_shp)
MDG_shp.df$number <- 1
mal_binded <- cbind(mal_pts, MDG_shp.df$number)
mal_binded_df <- data.frame("District" = mal_binded[,3], "Population" = mal_binded[,9])
mal_binded_df<- mal_binded_df %>% 
  mutate(District = toupper(District))

mal_poptable <- table(mal_binded_df$District)
mal_pop_df <- data.frame("District" = names(mal_poptable))
mal_pop_df$malaria_instance <- 0

for(i in 1:length(mal_poptable)){
  mal_name <- names(mal_poptable)[i]
  mal_cells <-mal_binded_df[which(mal_binded_df$District == mal_name),]
  mal_pop<- sum(mal_cells$Population,na.rm=TRUE)
  mal_pop_df[which(mal_pop_df$District == mal_name),]$malaria_instance <- mal_pop
}
mal_pop_dfs <- mal_pop_df[which(mal_pop_df$District %in% districts$DISTRICT),]

districts$malaria_instance <- 0
for(i in 1:NROW(mal_pop_dfs)){
  mal_cell <- mal_pop_dfs[i,]
  mal_cell$District <- sapply(mal_cell$District,solve_name)
  if(NROW(districts[which(districts$DISTRICT == toupper(mal_cell$District)),]$births) > 0){
    districts[which(districts$DISTRICT == toupper(mal_cell$District)),]$malaria_instance <- mal_cell$malaria_instance
  }
}

### Extract Distance to City Covariates and map them to the Tehsils Data

distance_dt <- get(load('VaccinationStudy/Data/distance_to_cities_data.Rdata'))
dis_df <- as.data.frame(distance_dt,xy=TRUE)

coordinates(dis_df)<- ~x +y
proj4string(dis_df) <- proj4string(tehsils_shp)

dist_pts <- over(dis_df, tehsils_shp)
dist_binded <- cbind(dist_pts, dis_df$accessibility_to_cities_2015_v1.0)
dist_binded_df <- data.frame("District" = dist_binded[,3],"Tehsil" = dist_binded[,4], "Population" = dist_binded[,9])
dist_binded_df<- dist_binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))

dist_binded_df[which(dist_binded_df$Tehsil == "SAHIWAL" & dist_binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

dist_poptable <- table(dist_binded_df$Tehsil)
dist_pop_df <- data.frame("Tehsil" = names(dist_poptable))
dist_pop_df$distance_to_cities <- 0

for(i in 1:length(dist_poptable)){
  dist_name <- names(dist_poptable)[i]
  dist_cells <-dist_binded_df[which(dist_binded_df$Tehsil == dist_name),]
  dist_pop<- mean(dist_cells$Population,na.rm=TRUE)
  dist_pop_df[which(dist_pop_df$Tehsil == dist_name),]$distance_to_cities <- dist_pop
}
tehsils$distance_to_cities <- 0
for(i in 1:NROW(dist_pop_df)){
  dist_cell <- dist_pop_df[i,]
  dist_cell$Tehsil <- sapply(dist_cell$Tehsil,solve_name)
  if(NROW(tehsils[which(tehsils$TEHSIL == toupper(dist_cell$Tehsil)),]$distance_to_cities) > 0){
    tehsils[which(tehsils$TEHSIL == toupper(dist_cell$Tehsil)),]$distance_to_cities <- dist_cell$distance_to_cities
  }
}

### Extract Distance to City Covariates and map them to the Districts Data

distance_dt <- get(load('VaccinationStudy/Data/distance_to_cities_data.Rdata'))
dis_df <- as.data.frame(distance_dt,xy=TRUE)

coordinates(dis_df)<- ~x +y
proj4string(dis_df) <- proj4string(districts_shp)

dist_pts <- over(dis_df, districts_shp)
dist_binded <- cbind(dist_pts, dis_df$accessibility_to_cities_2015_v1.0)
dist_binded_df <- data.frame("District" = dist_binded[,3], "Population" = dist_binded[,9])
dist_binded_df<- dist_binded_df %>% 
  mutate(Tehsil = toupper(District))

dist_poptable <- table(dist_binded_df$Tehsil)
dist_pop_df <- data.frame("District" = names(dist_poptable))
dist_pop_df$distance_to_cities <- 0

for(i in 1:length(dist_poptable)){
  dist_name <- names(dist_poptable)[i]
  dist_cells <-dist_binded_df[which(dist_binded_df$District == dist_name),]
  dist_pop<- mean(dist_cells$Population,na.rm=TRUE)
  dist_pop_df[which(dist_pop_df$District == dist_name),]$distance_to_cities <- dist_pop
}
districts$distance_to_cities <- 0
for(i in 1:NROW(dist_pop_df)){
  dist_cell <- dist_pop_df[i,]
  dist_cell$District <- sapply(dist_cell$District,solve_district_name)
  if(NROW(districts[which(districts$DISTRICT == toupper(dist_cell$District)),]$distance_to_cities) > 0){
    districts[which(districts$DISTRICT == toupper(dist_cell$District)),]$distance_to_cities <- dist_cell$distance_to_cities
  }
}


### Extract Population Covariate
# Note: This file is very large and will take awhile to run

total_population <- read.csv(file = 'VaccinationStudy/Data/population_pak_2018-10-01.csv')
tehsils$Population <- 0
districts$Population <- 0

coordinates(total_population)<- ~longitude +latitude
proj4string(total_population) <- proj4string(tehsils_shp)

## memory.limit(size=56000)
tehsil_pts <- over(total_population, tehsils_shp)

total_binded <- cbind(tehsil_pts, total_population$population_2020)

total_binded_df <- data.frame("District" = total_binded[,3], "Tehsil" = total_binded[,4], "Population" =  total_binded[,9])
total_binded_df<- total_binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))
total_binded_df<- total_binded_df %>% 
  mutate(District = toupper(District))

total_binded_df[which(total_binded_df$Tehsil == "SAHIWAL" & total_binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

total_poptable <- table(total_binded_df$Tehsil)
total_pop_df <- data.frame("Tehsil" = names(total_poptable))
total_pop_df$population <- 0

for(i in 1:length(total_poptable)){
  total_name <- names(total_poptable)[i]
  total_cells <- total_binded_df[which(total_binded_df$Tehsil == total_name),]
  total_pop<- sum(total_cells$Population,na.rm=TRUE)
  total_pop_df[which(total_pop_df$Tehsil == total_name),]$population <- total_pop
}
tehsils$Population <- NA
total_pop_dfs <- total_pop_df[which(total_pop_df$Tehsil %in% tehsils$TEHSIL),]
for(i in 1:NROW(total_pop_dfs)){
  cell <- total_pop_dfs[i,]
  name <- solve_name(cell$Tehsil)
  tehsils[which(tehsils$TEHSIL == toupper(name)),]$Population <- cell$population
}
 
### Extract Child Population Covariate
# Note: This file is very large and will take awhile to run

child_population <- read.csv(file = 'VaccinationStudy/Data/PAK_children_under_five_2019-08-03.csv')
coordinates(child_population)<- ~longitude +latitude
proj4string(child_population) <- proj4string(tehsils_shp)

pts <- over(child_population, tehsils_shp)

binded <- cbind(pts, child_population$population)

binded_df <- data.frame("District" = binded[,3], "Tehsil" = binded[,4], "Population" = binded[,9])
binded_df<- binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))
binded_df<- binded_df %>% 
  mutate(District = toupper(District))

binded_df[which(binded_df$Tehsil == "SAHIWAL" & binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"
poptable <- table(binded_df$Tehsil)
pop_df <- data.frame("Tehsil" = names(poptable))
pop_df$population <- 0

for(i in 1:length(poptable)){
  name <- names(poptable)[i]
  cells <- binded_df[which(binded_df$Tehsil == name),]
  pop<- sum(cells$Population,na.rm=TRUE)
  name <- solve_name(name)
  pop_df[which(pop_df$Tehsil == name),]$population <- pop
}
pop_df <- pop_df[which(pop_df$Tehsil %in% tehsils$TEHSIL),]
tehsils$child_population <- NA
for(i in 1:NROW(pop_df)){
  cell <- pop_df[i,]
  name <- solve_name(cell$Tehsil)
  tehsils[which(tehsils$TEHSIL == name),]$child_population <- cell$population
}

### Extract Population Covariate For Districts
# Note: This file is very large and will take awhile to run

coordinates(total_population)<- ~longitude +latitude
proj4string(total_population) <- proj4string(districts_shp)

pts <- over(total_population, districts_shp)

total_binded <- cbind(pts, total_population$population_2020)

total_binded_df <- data.frame("District" = total_binded[,3], "Population" =  total_binded[,9])
total_binded_df<- total_binded_df %>% 
  mutate(District = toupper(District))


total_poptable <- table(total_binded_df$District)
total_pop_df <- data.frame("District" = names(total_poptable))
total_pop_df$population <- 0

for(i in 1:length(total_poptable)){
  total_name <- names(total_poptable)[i]
  total_cells <- total_binded_df[which(total_binded_df$District == total_name),]
  total_pop<- sum(total_cells$Population,na.rm=TRUE)
  total_pop_df[which(total_pop_df$District == total_name),]$population <- total_pop
}
districts$Population <- NA
total_pop_dfs <- total_pop_df[which(total_pop_df$District %in% districts$District),]
for(i in 1:NROW(total_pop_dfs)){
  cell <- total_pop_dfs[i,]
  name <- solve_district_name(cell$District)    
  districts[which(districts$DISTRICT == toupper(name)),]$Population <- cell$population
}


### Extract and Map Child Population Covariate to Districts Data

coordinates(child_population)<- ~longitude +latitude
proj4string(child_population) <- proj4string(districts_shp)

pts <- over(child_population, districts_shp)

binded <- cbind(pts, child_population$population)

binded_df <- data.frame("District" = binded[,3], "Population" = binded[,9])
binded_df<- binded_df %>% 
  mutate(District = toupper(District))

poptable <- table(binded_df$District)
pop_df <- data.frame("District" = apply(data.frame(names(poptable)), 1, solve_district_name))
pop_df$population <- 0

for(i in 1:length(poptable)){
  name <- names(poptable)[i]
  cells <- binded_df[which(binded_df$District == name),]
  pop<- sum(cells$Population,na.rm=TRUE)
  name <- solve_district_name(name)
  pop_df[which(pop_df$District == name),]$population <- pop
}
pop_df <- pop_df[which(pop_df$District %in% districts$DISTRICT),]
districts$child_population <- NA
for(i in 1:NROW(pop_df)){
  cell <- pop_df[i,]
  name <- solve_district_name(cell$District)
  districts[which(districts$DISTRICT == name),]$child_population <- cell$population
}

### Get Population Density from existing columns and add to Tehsil and District Level Data as a Covariate

tehsils$population_density <- 0
districts$population_density <- 0

tehsils$population_density <- tehsils$Population / tehsils$Shape_Area
districts$population_density <- districts$Population / districts$Shape_Area

write.csv(tehsils, "D:\\Xiaoting\\VaccinationProject\\tehsils_assemble.csv")
write.csv(districts, "D:\\Xiaoting\\VaccinationProject\\districts_assemble.csv")

