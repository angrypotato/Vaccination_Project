## revised from the github version

## Purpose: To Extract Vaccination Data and Map To Tehsil and District Level Geographic Data 

## Note that these Files are Large and Data Manipulation May Take a While


# Pre ----

## Call Source File for Required Libraries and Functions 

source(file='PreRunNew.r')

## Read in Past EPI Level Extract Files to Get Vaccination Data and Combine Them

epi_files_17 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2017/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_17 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2017/Non-EPI-Updated", full.names = T)
epi_files_18 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2018/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_18 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2018/Non-EPI-Updated", full.names = T)
epi_files_19 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2019/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_19 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2019/Non-EPI-Updated", full.names = T)

epi_files <- c(epi_files_17,non_epi_files_17,epi_files_18,non_epi_files_18,
               epi_files_19,non_epi_files_19)


# Read in Data Regarding Punjab Clinics - the goal is to differentiate between vaccinations given at clinics vs those done via outreach

facilities <- readxl::read_xls('VaccinationStudy/Data/Facilities_location.xls')

## 200m buffer (approximately)

facilities$latitude_high <- facilities$latitude + .0015
facilities$latitude_low <- facilities$latitude - .0015
facilities$longitude_high <- facilities$longitude + .0015
facilities$longitude_low <- facilities$longitude - .0015


# Prepare the variables to be filled in via the EPI Files

in_clinics <- 0
out_clinics <- 0
tot.instance.penta3 <- 0

tehsils$penta3_in_clinic <- 0
tehsils$penta3_out_clinic <- 0

ucs$penta3_in_clinic <- 0
ucs$penta3_out_clinic <- 0

facilities$penta3 <- 0
facilities$TEHSIL <- ""
facilities$in_clinic <- 0
facilities$out_clinic <- 0




# Extract Penta Vacc Stats for Tehsils and Districts ---- 



## tehsil ----

clean_df <- function(fl){
  file1 <- read.csv(fl)
  file1 <- file1[,-10]
  file1 <- file1[!duplicated(file1), ]
  file1 <- file1 %>%
    separate(location, into = c('lat', 'long'), sep=",")
  file1$has_tehsil <- 1
  file1[(file1$town_name == "" | file1$town_name == "NULL"),]$has_tehsil <- 0
  file1 <- file1[grepl('penta',file1$Vaccination,ignore.case=TRUE),]
  no_town <- file1[(file1$has_tehsil == 0),]
  no_town$valid <- 1
  no_town$valid[no_town$lat == "0.0"] <- 0
  no_town$valid[grep("-", no_town$lat)] <- 0
  no_town$valid[no_town$long == "0.0"] <- 0
  no_town$valid[grep("-", no_town$long)] <- 0
  no_town$valid[grep("\n", no_town$long)] <- 0
  no_town$valid[grep("\n", no_town$lat)] <- 0
  no_town <- transform(no_town, lat = as.numeric(lat),
                       long = as.numeric(long))
  no_town$valid[no_town$long < 60  || no_town$lat < 20 || no_town$long >90 || no_town$lat > 50] <- 0
  use_coords <- no_town[(no_town$valid == 1),]
  use_coords <- na.exclude(use_coords)
  coordinates(use_coords)<- ~long +lat
  proj4string(use_coords) <- proj4string(tehsils_shp)
  f_pts <- over(use_coords, tehsils_shp)
  use_tehsil <- file1[(file1$has_tehsil == 1),]
  new_use_coords <- f_pts[,c(3,4)]
  new_use_coords <- cbind(new_use_coords, use_coords$lat, use_coords$long)
  new_use_tehs <- use_tehsil[,c(2,3,5,6,7)]
  new_use_coords <- cbind(new_use_coords,use_coords$Vaccination)
  colnames(new_use_tehs)[colnames(new_use_tehs)=="town_name"] <- "TEHSIL"
  colnames(new_use_tehs)[colnames(new_use_tehs)=="district_name"] <- "DISTRICT"
  colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$Vaccination"] <- "Vaccination"
  colnames(new_use_tehs)[colnames(new_use_tehs) == "daily_reg_no"] <- "Vaccination"
  colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$lat"] <- "lat"
  colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$long"] <- "long"
  new_use_coords[complete.cases(new_use_coords$TEHSIL),]
}


for(file in 1:length(epi_files)){
  f <- clean_df(epi_files[file])
  f$Vaccination <- tolower(f$Vaccination)
  f$TEHSIL <- toupper(f$TEHSIL)
  
  f$has_penta3 <- 0
  f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(f$Vaccination)),1,0)
  tot.instance.penta3 <- tot.instance.penta3 + sum(f$has_penta3)  # global index
  
  in_clinics <- tehsils$penta3_in_clinic   # local index recording data before running this file
  
  ### in clinic 
  for(fa in 1:NROW(facilities)){
    fac <- facilities[fa,]
    name <- fac$facility_name
    
    # filter obs in the facility radius
    clinic_f <- f[which(f$long >= fac$longitude_low & f$long <= fac$longitude_high
                        & f$lat <= fac$latitude_high & f$lat >= fac$latitude_low),]
    
    num_clinic <- NROW(clinic_f)
    if (num_clinic >0) {
      facilities[which(facilities$facility_name == name),]$in_clinic <- facilities[(facilities$facility_name == name),]$in_clinic + num_clinic
      
      num_teh <- length(unique(clinic_f$TEHSIL))   # for different tehsils in the clinic range
      for (t in 1:num_teh) {
        tehs <- unique(clinic_f$TEHSIL)[t]
        
        instance.penta3 <- sum(clinic_f[which(clinic_f$TEHSIL == tehs),]$has_penta3)  
        # instance of inclinic_penta3 in each tehsil
        
        tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      }
    }  
  }
  
  # outreach
  for(k in 1:NROW(tehsils)) { 
    tehs <- tehsils$TEHSIL[k]
    if(is.na(tehs)){
      next
    }
    
    ftable <- f[which((f$TEHSIL == tehs) & (f$has_penta3 == 1)),]
    fpenta3<- sum(ftable$has_penta3)
    ### in_clinic obs in this file of this tehsil
    penta3_out <- fpenta3 - (tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic - in_clinics[k])
    
    tehsils[which(tehsils$TEHSIL == tehs),]$penta3_out_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta3_out_clinic + penta3_out
  }
  
}



### uc ----

uc_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs@data$id <- rownames(ucs@data)
ucs <- data.frame(ucs)
ucs <- ucs[which(ucs$PROVINCE == 'Punjab'),] %>%
  mutate(UC = toupper(UC))

clean_df_uc <- function(fl){
  file1 <- read.csv(fl)
  file1 <- file1[,-10]
  file1 <- file1[!duplicated(file1), ]
  file1 <- file1 %>%
    separate(location, into = c('lat', 'long'), sep=",")
  file1$has_uc <- 1
  file1[which(file1$uc_name == "" | file1$uc_name == "NULL"),]$has_uc <- 0
  file1 <- file1[grepl('Pentavalent-3',file1$Vaccination,ignore.case=TRUE),]
  no_uc <- file1[(file1$has_uc == 0),]
  no_uc$valid <- 1
  no_uc$valid[no_uc$lat == "0.0"] <- 0
  no_uc$valid[grep("-", no_uc$lat)] <- 0
  no_uc$valid[no_uc$long == "0.0"] <- 0
  no_uc$valid[grep("-", no_uc$long)] <- 0
  no_uc$valid[grep("\n", no_uc$long)] <- 0
  no_uc$valid[grep("\n", no_uc$lat)] <- 0
  no_uc <- transform(no_uc, lat = as.numeric(lat),
                       long = as.numeric(long))
  no_uc$valid[no_uc$long < 60  || no_uc$lat < 20 || no_uc$long >90 || no_uc$lat > 50] <- 0
  use_coords <- no_uc[(no_uc$valid == 1),]
  use_coords <- na.exclude(use_coords)
  coordinates(use_coords)<- ~long +lat
  proj4string(use_coords) <- proj4string(uc_shp)
  f_pts <- over(use_coords, uc_shp)
  use_uc <- file1[(file1$has_uc == 1),]
  new_use_coords <- f_pts[,c(3,4)]
  new_use_coords <- cbind(new_use_coords, use_coords$lat, use_coords$long)
  new_use_uc <- use_uc[,c(3,4,5,6,7)]
  new_use_coords <- cbind(new_use_coords,use_coords$Vaccination)
  colnames(new_use_uc)[colnames(new_use_uc)=="uc_name"] <- "UC"
  colnames(new_use_uc)[colnames(new_use_uc)=="town_name"] <- "TEHSIL"
  colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$Vaccination"] <- "Vaccination"
  colnames(new_use_uc)[colnames(new_use_uc) == "daily_reg_no"] <- "Vaccination"
  colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$lat"] <- "lat"
  colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$long"] <- "long"
  vaccs_data <- rbind(new_use_coords,new_use_uc)
  vaccs_data[complete.cases(vaccs_data$UC),]
}



epi_files_new <- epi_files[-c(24,37,43)]
for(file in 1:length(epi_files_new)){
  f <- clean_df_uc(epi_files_new[file])
  f$Vaccination <- tolower(f$Vaccination)
  f$UC <- toupper(f$UC)
  
  f$has_penta3 <- 0
  f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(f$Vaccination)),1,0)
  tot.instance.penta3 <- tot.instance.penta3 + sum(f$has_penta3)  # global index
  
  in_clinics <- ucs$penta3_in_clinic   # local index recording data before running this file
  
  ### in clinic 
  for(fa in 1:NROW(facilities)){
    fac <- facilities[fa,]
    name <- fac$facility_name
    
    # filter obs in the facility radius
    clinic_f <- f[which(f$long >= fac$longitude_low & f$long <= fac$longitude_high
                        & f$lat <= fac$latitude_high & f$lat >= fac$latitude_low),]
    
    num_clinic <- NROW(clinic_f)
    if (num_clinic >0) {
     
      num_uc <- length(unique(clinic_f$UC))   # for different ucs in the clinic range
      for (t in 1:num_uc) {
        ucname <- unique(clinic_f$UC)[t]
        
        instance.penta3 <- sum(clinic_f[which(clinic_f$UC == ucname),]$has_penta3)  
        # instance of inclinic_penta3 in each UC
        
        ucs[which(ucs$UC == ucname),]$penta3_in_clinic <- ucs[which(ucs$UC == ucname),]$penta3_in_clinic + instance.penta3
      }
    }  
  }
  
  # outreach
  for(k in 1:NROW(ucs)) { 
    ucname <- ucs$UC[k]
    if(is.na(ucname)){
      next
    }
    
    ftable <- f[which((f$UC == ucname) & (f$has_penta3 == 1)),]
    fpenta3<- sum(ftable$has_penta3)
    ### in_clinic obs in this file of this tehsil
    penta3_out <- fpenta3 - (ucs[which(ucs$UC == ucname),]$penta3_in_clinic - in_clinics[k])
    
    ucs[which(ucs$UC == ucname),]$penta3_out_clinic <- ucs[which(ucs$UC == ucname),]$penta3_out_clinic + penta3_out
  }
  print(file)
}

 # results/uc_vacc.csv



# merge outcome and covariates ----
ucs_outcome <- read.csv("results/ucs_vacc.csv")[,c(3:5,22,23)] # outcome variable
ucs_covar <- read.csv("results/ucs_covariates.csv")  # covariates
ucs_complete <- merge(ucs_covar, ucs_outcome, by = c("UC", "DISTRICT","TEHSIL"), all.x = T) %>%
  mutate(OutreachProportion = penta3_out_clinic / (penta3_in_clinic + penta3_out_clinic),
         TotalOutreachCoverage = penta3_out_clinic / child_population,  
         TotalClinicsCoverage = penta3_in_clinic / child_population)
# write.csv(ucs_complete, "results/ucs_complete.csv")

tehsils_covar <- read.csv("results/tehsils_covar.csv")
tehsils_outcome <- read.csv("results/tehsils_vacc.csv")[, c(4,5,12,14)] 
tehsils_complete <- merge(tehsils_covar, tehsils_outcome, by = c("DISTRICT","TEHSIL"), all.x = T) %>%
  mutate(OutreachProportion = penta3_out_clinic / (penta3_in_clinic+penta3_out_clinic),
         TotalOutreachCoverage = penta3_out_clinic / child_population,
         TotalClinicsCoverage = penta3_in_clinic / child_population)
tehsils_complete_data <- tehsils_complete[,c(6,2,1,12:21,24,27,30,33,39,42,45,48,51,57,67,70:72)]
# write.csv(tehsils_complete_data, "results/tehsils_complete_new.csv")

