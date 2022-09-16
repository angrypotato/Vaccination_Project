## revised from the github version

## Purpose: To Extract Vaccination Data and Map To Tehsil and District Level Geographic Data 

## Note that these Files are Large and Data Manipulation May Take a While


# Pre ----

## Call Source File for Required Libraries and Functions 

source(file='PreRunNew.r')


## read in tehsil data

tehsils_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils@data$id <- rownames(tehsils@data)
tehsils <- data.frame(tehsils)
tehsils <- tehsils[which(tehsils$PROVINCE == 'PUNJAB'),]
tehsils$TEHSIL <- sapply(tehsils$TEHSIL,solve_name)
tehsils <- tehsils[!(tehsils$TEHSIL %in% c('RAZMAK')),]   ### 136 obs left
tehsils[which(tehsils$TEHSIL == "SAHIWAL" & tehsils$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"



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
  file1 <- file1[grepl('pentavalent-3',file1$Vaccination,ignore.case=TRUE),]
  
  ### use town name
  use_tehsil <- file1[(file1$has_tehsil == 1),]
  new_use_tehs <- use_tehsil[,c(2,3,5,6,7)]
  colnames(new_use_tehs)[colnames(new_use_tehs)=="town_name"] <- "TEHSIL"
  colnames(new_use_tehs)[colnames(new_use_tehs)=="district_name"] <- "DISTRICT"
  colnames(new_use_tehs)[colnames(new_use_tehs) == "daily_reg_no"] <- "Vaccination"
  
  ### use lat+long
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
  
  if (nrow(no_town[(no_town$valid == 1),]) > 0) {
    use_coords <- no_town[(no_town$valid == 1),]
    use_coords <- na.exclude(use_coords)
    coordinates(use_coords)<- ~long +lat
    proj4string(use_coords) <- proj4string(tehsils_shp)
    f_pts <- over(use_coords, tehsils_shp)
    
    new_use_coords <- f_pts[,c(3,4)]
    new_use_coords <- cbind(new_use_coords, use_coords$lat, use_coords$long)
    new_use_coords <- cbind(new_use_coords,use_coords$Vaccination)
    colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$Vaccination"] <- "Vaccination"
    colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$lat"] <- "lat"
    colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$long"] <- "long"

    vaccs_data <- rbind(new_use_coords,new_use_tehs)
    
  } else {
    vaccs_data <- new_use_tehs[complete.cases(new_use_tehs$TEHSIL),]
  }
  
  vaccs_data <- vaccs_data[complete.cases(vaccs_data$TEHSIL),] 
  vaccs_data$TEHSIL <- sapply(vaccs_data$TEHSIL,solve_name)
  vaccs_data <- vaccs_data[!(vaccs_data$TEHSIL %in% c('RAZMAK')),] 
  vaccs_data[which(vaccs_data$TEHSIL == "SAHIWAL" & vaccs_data$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"
  
  vaccs_data
}




for(file in 1:length(epi_files)){
  f <- clean_df(epi_files[file])
  f$Vaccination <- tolower(f$Vaccination)
  
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
  print(file)
} 

write.csv(tehsils, "results/tehsil_vacc_9.12.csv")



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
tehsils_complete <- merge(tehsils_covar, tehsils_outcome, by = c("TEHSIL"), all.x = T) %>%
  mutate(OutreachProportion = penta3_out_clinic / (penta3_in_clinic+penta3_out_clinic),
         TotalOutreachCoverage = penta3_out_clinic / child_population,
         TotalClinicsCoverage = penta3_in_clinic / child_population)
tehsils_complete_data <- tehsils_complete[,c(6,2,1,12:21,24,27,30,33,39,42,45,48,51,57,67,70:72)]
# write.csv(tehsils_complete_data, "results/tehsils_complete_new.csv")



#### 7/19 merge newest data
covar <- read.csv("results/tehsils_mics_7.14.csv")[,c(2,12:20,23,26,29,32,38,44,47,50,56,66)]
vacc <- read.csv("results/tehsil_vacc_7.18.csv")[,c(5,11,12)]
fac <- read.csv("results/tehsils_fac_number.csv")[,c(2,10)]
binded <- merge(covar, vacc, by = "TEHSIL", all.x = T)
tehsil_complete <- merge(binded, fac, by = "TEHSIL", all.x = T) %>%
  mutate(OutreachProportion = penta3_out_clinic / (penta3_in_clinic + penta3_out_clinic),
         TotalOutreachCoverage = penta3_out_clinic / child_population,  
         TotalClinicsCoverage = penta3_in_clinic / child_population) 
write.csv(tehsil_complete, "results/tehsils_complete_7.19.csv")





# tehsil clinic number ----

tehsils_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Tehsil_Boundary.shp")
tehsils <- data.frame(tehsils)
tehsils <- tehsils[which(tehsils$PROVINCE == 'PUNJAB'),]
tehsils$TEHSIL <- sapply(tehsils$TEHSIL,solve_name)
tehsils <- tehsils[!(tehsils$TEHSIL %in% c('RAZMAK')),]   ### 137 obs left
tehsils[which(tehsils$TEHSIL == "SAHIWAL" & tehsils$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL"


coordinates(facilities)<- ~longitude +latitude
proj4string(facilities) <- proj4string(tehsils_shp)
fac_points <- over(facilities, tehsils_shp)
facilities$number <- 1
fac_binded <- cbind(fac_points, facilities$number)
fac_binded_df <- data.frame("District" = fac_binded[,3],"Tehsil" = fac_binded[,4], "Population" = fac_binded[,9])
fac_binded_df<- fac_binded_df %>% 
  mutate(Tehsil = toupper(Tehsil))

fac_binded_df[which(fac_binded_df$Tehsil == "SAHIWAL" & fac_binded_df$District == "SAHIWAL"),]$Tehsil <- "SAHIWAL_SAHIWAL"

fac_binded_df$Tehsil <- sapply(fac_binded_df$Tehsil,solve_name)
fac_binded_df <- fac_binded_df %>%
  group_by(Tehsil) %>%
  summarise(fac_number = sum(Population)) %>%
  rename(TEHSIL = Tehsil)

tehsils <- merge(tehsils[,c(2,10)], tehsils2[,-30], by="TEHSIL", all.x = T)
write.csv(tehsils, "results/tehsils_complete_7.15.csv")




# tehsil result by year ----


## 2017

# epi_files <- epi_files[-c(1:7,13:19)]
epi_17 <- epi_files[1:10]
tehsils_17 <- tehsils

for(file in 1:length(epi_17)){
  f <- clean_df(epi_17[file])
  f$Vaccination <- tolower(f$Vaccination)
  
  f$has_penta3 <- 0
  f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(f$Vaccination)),1,0)
  tot.instance.penta3 <- tot.instance.penta3 + sum(f$has_penta3)  # global index
  
  in_clinics <- tehsils_17$penta3_in_clinic   # local index recording data before running this file
  
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
        
        tehsils_17[(tehsils_17$TEHSIL == tehs),]$penta3_in_clinic <- tehsils_17[(tehsils_17$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      }
    }  
  }
  
  # outreach
  for(k in 1:NROW(tehsils_17)) { 
    tehs <- tehsils_17$TEHSIL[k]
    if(is.na(tehs)){
      next
    }
    
    ftable <- f[which((f$TEHSIL == tehs) & (f$has_penta3 == 1)),]
    fpenta3<- sum(ftable$has_penta3)
    ### in_clinic obs in this file of this tehsil
    penta3_out <- fpenta3 - (tehsils_17[(tehsils_17$TEHSIL == tehs),]$penta3_in_clinic - in_clinics[k])
    
    tehsils_17[which(tehsils_17$TEHSIL == tehs),]$penta3_out_clinic <- tehsils_17[(tehsils_17$TEHSIL == tehs),]$penta3_out_clinic + penta3_out
  }
  print(file)
} 

write.csv(tehsils_17, "results/tehsil_vacc_17_new.csv")


## 2018

epi_18 <- c(epi_files_18,non_epi_files_18)
tehsils_18 <- tehsils

for(file in 1:length(epi_18)){
  f <- clean_df(epi_18[file])
  f$Vaccination <- tolower(f$Vaccination)
  
  f$has_penta3 <- 0
  f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(f$Vaccination)),1,0)
  tot.instance.penta3 <- tot.instance.penta3 + sum(f$has_penta3)  # global index
  
  in_clinics <- tehsils_18$penta3_in_clinic   # local index recording data before running this file
  
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
        
        tehsils_18[(tehsils_18$TEHSIL == tehs),]$penta3_in_clinic <- tehsils_18[(tehsils_18$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      }
    }  
  }
  
  # outreach
  for(k in 1:NROW(tehsils_18)) { 
    tehs <- tehsils_18$TEHSIL[k]
    if(is.na(tehs)){
      next
    }
    
    ftable <- f[which((f$TEHSIL == tehs) & (f$has_penta3 == 1)),]
    fpenta3<- sum(ftable$has_penta3)
    ### in_clinic obs in this file of this tehsil
    penta3_out <- fpenta3 - (tehsils_18[(tehsils_18$TEHSIL == tehs),]$penta3_in_clinic - in_clinics[k])
    
    tehsils_18[which(tehsils_18$TEHSIL == tehs),]$penta3_out_clinic <- tehsils_18[(tehsils_18$TEHSIL == tehs),]$penta3_out_clinic + penta3_out
  }
  print(file)
} 

write.csv(tehsils_18, "results/tehsil_vacc_18.csv")



## 2019

epi_19 <- c(epi_files_19,non_epi_files_19)
tehsils_19 <- tehsils

for(file in 1:length(epi_19)){
  f <- clean_df(epi_19[file])
  f$Vaccination <- tolower(f$Vaccination)
  
  f$has_penta3 <- 0
  f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(f$Vaccination)),1,0)
  tot.instance.penta3 <- tot.instance.penta3 + sum(f$has_penta3)  # global index
  
  in_clinics <- tehsils_19$penta3_in_clinic   # local index recording data before running this file
  
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
        
        tehsils_19[(tehsils_19$TEHSIL == tehs),]$penta3_in_clinic <- tehsils_19[(tehsils_19$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      }
    }  
  }
  
  # outreach
  for(k in 1:NROW(tehsils_19)) { 
    tehs <- tehsils_19$TEHSIL[k]
    if(is.na(tehs)){
      next
    }
    
    ftable <- f[which((f$TEHSIL == tehs) & (f$has_penta3 == 1)),]
    fpenta3<- sum(ftable$has_penta3)
    ### in_clinic obs in this file of this tehsil
    penta3_out <- fpenta3 - (tehsils_19[(tehsils_19$TEHSIL == tehs),]$penta3_in_clinic - in_clinics[k])
    
    tehsils_19[which(tehsils_19$TEHSIL == tehs),]$penta3_out_clinic <- tehsils_19[(tehsils_19$TEHSIL == tehs),]$penta3_out_clinic + penta3_out
  }
  print(file)
} 

write.csv(tehsils_19, "results/tehsil_vacc_19.csv")


sum(tehsils_17$penta3_in_clinic)
sum(tehsils_17$penta3_out_clinic)
sum(tehsils_17$penta3_out_clinic)/(sum(tehsils_17$penta3_out_clinic)+sum(tehsils_17$penta3_in_clinic))

sum(tehsils_18$penta3_in_clinic)
sum(tehsils_18$penta3_out_clinic)
sum(tehsils_18$penta3_out_clinic)/(sum(tehsils_18$penta3_out_clinic)+sum(tehsils_18$penta3_in_clinic))

sum(tehsils_19$penta3_in_clinic)
sum(tehsils_19$penta3_out_clinic)
sum(tehsils_19$penta3_out_clinic)/(sum(tehsils_19$penta3_out_clinic)+sum(tehsils_19$penta3_in_clinic))

tehsils_all <- read.csv("results/tehsils_complete_7.19.csv")
sum(sum(tehsils_17$penta3_in_clinic) + sum(tehsils_18$penta3_in_clinic) + sum(tehsils_19$penta3_in_clinic)) == sum(tehsils_all$penta3_in_clinic)  
sum(sum(tehsils_17$penta3_out_clinic) + sum(tehsils_18$penta3_out_clinic) + sum(tehsils_19$penta3_out_clinic)) == sum(tehsils_all$penta3_out_clinic)  



# test facility buffer ----
## use a random 5% of data across all time period

clean_df_buffer <- function(fl){
  file1 <- fl[,-10]
  file1 <- file1[!duplicated(file1), ]
  file1 <- file1 %>%
    separate(location, into = c('lat', 'long'), sep=",")
  file1$has_tehsil <- 1
  file1[(file1$town_name == "" | file1$town_name == "NULL"),]$has_tehsil <- 0
  file1 <- file1[grepl('pentavalent-3',file1$Vaccination,ignore.case=TRUE),]
  
  ### use town name
  use_tehsil <- file1[(file1$has_tehsil == 1),]
  new_use_tehs <- use_tehsil[,c(2,3,5,6,7)]
  colnames(new_use_tehs)[colnames(new_use_tehs)=="town_name"] <- "TEHSIL"
  colnames(new_use_tehs)[colnames(new_use_tehs)=="district_name"] <- "DISTRICT"
  colnames(new_use_tehs)[colnames(new_use_tehs) == "daily_reg_no"] <- "Vaccination"
  
  ### use lat+long
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
  
  if (nrow(no_town[(no_town$valid == 1),]) > 0) {
    use_coords <- no_town[(no_town$valid == 1),]
    use_coords <- na.exclude(use_coords)
    coordinates(use_coords)<- ~long +lat
    proj4string(use_coords) <- proj4string(tehsils_shp)
    f_pts <- over(use_coords, tehsils_shp)
    
    new_use_coords <- f_pts[,c(3,4)]
    new_use_coords <- cbind(new_use_coords, use_coords$lat, use_coords$long)
    new_use_coords <- cbind(new_use_coords,use_coords$Vaccination)
    colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$Vaccination"] <- "Vaccination"
    colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$lat"] <- "lat"
    colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$long"] <- "long"
    
    vaccs_data <- rbind(new_use_coords,new_use_tehs)
    
  } else {
    vaccs_data <- new_use_tehs[complete.cases(new_use_tehs$TEHSIL),]
  }
  
  vaccs_data <- vaccs_data[complete.cases(vaccs_data$TEHSIL),] 
  vaccs_data$TEHSIL <- sapply(vaccs_data$TEHSIL,solve_name)
  vaccs_data <- vaccs_data[!(vaccs_data$TEHSIL %in% c('RAZMAK')),] 
  if (length(which(vaccs_data$TEHSIL == "SAHIWAL" & vaccs_data$DISTRICT == "SAHIWAL")) > 0) {
    vaccs_data[which(vaccs_data$TEHSIL == "SAHIWAL" & vaccs_data$DISTRICT == "SAHIWAL"),]$TEHSIL <- "SAHIWAL_SAHIWAL" 
  }
  
  vaccs_data
}


facilities$latitude_high <- facilities$latitude + .001
facilities$latitude_low <- facilities$latitude - .001
facilities$longitude_high <- facilities$longitude + .001
facilities$longitude_low <- facilities$longitude - .001

facilities$penta3 <- 0
facilities$TEHSIL <- ""
facilities$in_clinic <- 0
facilities$out_clinic <- 0

epi_test <- epi_files[-c(1:8,13:20)]
tehsils_test <- tehsils


for(file in 1:length(epi_test)){
  
  # subset
  file1 <- read.csv(epi_test[file])
  set.seed(1)
  file2 <- slice_sample(file1, prop =0.05) 
  
  f <- clean_df_buffer(file2)
  f$Vaccination <- tolower(f$Vaccination)
  
  f$has_penta3 <- 0
  f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(f$Vaccination)),1,0)
  tot.instance.penta3 <- tot.instance.penta3 + sum(f$has_penta3)  # global index
  
  in_clinics <- tehsils_test$penta3_in_clinic   # local index recording data before running this file
  
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
        
        tehsils_test[(tehsils_test$TEHSIL == tehs),]$penta3_in_clinic <- tehsils_test[(tehsils_test$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      }
    }  
  }
  
  # outreach
  for(k in 1:NROW(tehsils_test)) { 
    tehs <- tehsils_test$TEHSIL[k]
    if(is.na(tehs)){
      next
    }
    
    ftable <- f[which((f$TEHSIL == tehs) & (f$has_penta3 == 1)),]
    fpenta3<- sum(ftable$has_penta3)
    ### in_clinic obs in this file of this tehsil
    penta3_out <- fpenta3 - (tehsils_test[(tehsils_test$TEHSIL == tehs),]$penta3_in_clinic - in_clinics[k])
    
    tehsils_test[which(tehsils_test$TEHSIL == tehs),]$penta3_out_clinic <- tehsils_test[(tehsils_test$TEHSIL == tehs),]$penta3_out_clinic + penta3_out
  }
  print(file)
} 


buffer_10 <- c(sum(tehsils_test$penta3_in_clinic), sum(tehsils_test$penta3_out_clinic))
buffer_15 <- c(sum(tehsils_test$penta3_in_clinic), sum(tehsils_test$penta3_out_clinic))
buffer_20 <- c(sum(tehsils_test$penta3_in_clinic), sum(tehsils_test$penta3_out_clinic))
buffer_25 <- c(sum(tehsils_test$penta3_in_clinic), sum(tehsils_test$penta3_out_clinic))



# 2017 by month ----

jan_17 <- epi_files[c(1,13)]
feb_17 <- epi_files[c(2,14)]
mar_17 <- epi_files[c(3,15)]
apr_17 <- epi_files[c(4,16)]
may_17 <- epi_files[c(5,17)]
jun_17 <- epi_files[c(6,18)]
jul_17 <- epi_files[c(7,19)]

aug_17 <- epi_files[c(8,20)]
sep_17 <- epi_files[c(9,21)]
oct_17 <- epi_files[c(10,22)]
nov_17 <- epi_files[c(11,23)]
dec_17 <- epi_files[c(12,24)]

jan_17_outcome <- tehsils
feb_17_outcome <- tehsils
mar_17_outcome <- tehsils
apr_17_outcome <- tehsils
may_17_outcome <- tehsils
jun_17_outcome <- tehsils
jul_17_outcome <- tehsils

aug_17_outcome <- tehsils
sep_17_outcome <- tehsils
oct_17_outcome <- tehsils
nov_17_outcome <- tehsils
dec_17_outcome <- tehsils


for(file in 1:length(jul_17)){
  f <- clean_df(jul_17[file])
  f$Vaccination <- tolower(f$Vaccination)
  
  f$has_penta3 <- 0
  f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(f$Vaccination)),1,0)
  tot.instance.penta3 <- tot.instance.penta3 + sum(f$has_penta3)  # global index
  
  in_clinics <- jul_17_outcome$penta3_in_clinic   # local index recording data before running this file
  
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
        
        jul_17_outcome[(jul_17_outcome$TEHSIL == tehs),]$penta3_in_clinic <- jul_17_outcome[(jul_17_outcome$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      }
    }  
  }
  
  # outreach
  for(k in 1:NROW(jul_17_outcome)) { 
    tehs <- jul_17_outcome$TEHSIL[k]
    if(is.na(tehs)){
      next
    }
    
    ftable <- f[which((f$TEHSIL == tehs) & (f$has_penta3 == 1)),]
    fpenta3<- sum(ftable$has_penta3)
    ### in_clinic obs in this file of this tehsil
    penta3_out <- fpenta3 - (jul_17_outcome[(jul_17_outcome$TEHSIL == tehs),]$penta3_in_clinic - in_clinics[k])
    
    jul_17_outcome[which(jul_17_outcome$TEHSIL == tehs),]$penta3_out_clinic <- jul_17_outcome[(jul_17_outcome$TEHSIL == tehs),]$penta3_out_clinic + penta3_out
  }
  print(file)
} 

mos_17 <- data.frame(month = c("jan","feb","mar", "apr","may","jun", "jul","aug","sep","oct","nov","dec"),
                     in_clinic = NA,
                     outreach = NA)
mos_17[1,c(2:4)] <- c(sum(jan_17_outcome$penta3_in_clinic),sum(jan_17_outcome$penta3_out_clinic))
mos_17[2,c(2:4)] <- c(sum(feb_17_outcome$penta3_in_clinic),sum(feb_17_outcome$penta3_out_clinic))
mos_17[3,c(2:4)] <- c(sum(mar_17_outcome$penta3_in_clinic),sum(mar_17_outcome$penta3_out_clinic))
mos_17[4,c(2:4)] <- c(sum(apr_17_outcome$penta3_in_clinic),sum(apr_17_outcome$penta3_out_clinic))
mos_17[5,c(2:4)] <- c(sum(may_17_outcome$penta3_in_clinic),sum(may_17_outcome$penta3_out_clinic))
mos_17[6,c(2:4)] <- c(sum(jun_17_outcome$penta3_in_clinic),sum(jun_17_outcome$penta3_out_clinic))
mos_17[7,c(2:4)] <- c(sum(jul_17_outcome$penta3_in_clinic),sum(jul_17_outcome$penta3_out_clinic))
mos_17[8,c(2:4)] <- c(sum(aug_17_outcome$penta3_in_clinic),sum(aug_17_outcome$penta3_out_clinic))
mos_17[9,c(2:4)] <- c(sum(sep_17_outcome$penta3_in_clinic),sum(sep_17_outcome$penta3_out_clinic))
mos_17[10,c(2:4)] <- c(sum(oct_17_outcome$penta3_in_clinic),sum(oct_17_outcome$penta3_out_clinic))
mos_17[11,c(2:4)] <- c(sum(nov_17_outcome$penta3_in_clinic),sum(nov_17_outcome$penta3_out_clinic))
mos_17[12,c(2:4)] <- c(sum(dec_17_outcome$penta3_in_clinic),sum(dec_17_outcome$penta3_out_clinic))

mos_17 <- mos_17 %>%
  mutate(proportion = outreach/(outreach+in_clinic),
         total = in_clinic+outreach)


## merge outcome (sep 17 - 19) and covar ----

tehsils_complete_8.15 <- read.csv("results/tehsils_complete_8.15.csv")
tehsils_proportion <- read.csv("results/proportion_9.15.csv")[, c(5,13)] 
tehsils_complete_9.15 <- merge(tehsils_complete_8.15, tehsils_proportion, by = c("TEHSIL"), all.x = T) %>%
  mutate(OutreachProportion = proportion)

tehsils_complete_9.15 <- tehsils_complete_9.15[,-c(2,31)]
write.csv(tehsils_complete_9.15, "results/tehsils_complete_9.15.csv")
