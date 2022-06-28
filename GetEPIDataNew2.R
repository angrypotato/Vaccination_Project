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

## Function to clean Individual EPI Files to ready them for Joining With Geographic Data
## output location and vacc info for each month
## fl = Single EPI File

clean_df <- function(fl){
  file1 <- read.csv(fl)
  file1 <- file1[,-10]
  file1 <- file1[!duplicated(file1), ]
  file1 <- file1 %>%
    separate(location, into = c('lat', 'long'), sep=",")
  file1$has_tehsil <- 1
  file1[(file1$town_name == ""),]$has_tehsil <- 0
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




# Extract Penta Vacc Stats for Tehsils and Districts ---- 

# Note that Penta1 = Pentavalent Vaccine Dose 1; Penta2 = Pentavalent Dose 2; Penta3 = Pentavalent Dose 3

# For Each EPI File, Join Tehsil and District Level Data to Pentavalent Vaccination Data

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
tehsils$penta1_in_clinic <- 0
tehsils$penta3_in_clinic <- 0
tehsils$penta1_out_clinic <- 0
tehsils$penta3_out_clinic <- 0

ucs$penta1_in_clinic <- 0
ucs$penta3_in_clinic <- 0
ucs$penta1_out_clinic <- 0
ucs$penta3_out_clinic <- 0

facilities$penta1 <- 0
facilities$penta3 <- 0
facilities$TEHSIL <- ""
facilities$in_clinic <- 0
facilities$out_clinic <- 0

# Get Clinics and Outreach Penta Vaccination data for each Punjab Tehsil

for(file in 1:length(epi_files)){
  f <- clean_df2(epi_files[file])
  f$Vaccination <- tolower(f$Vaccination)
  f$TEHSIL <- toupper(f$TEHSIL)
  
  ## in clinic
  for(fa in 1:NROW(facilities)){
    fac <- facilities[fa,]
    name <- fac$facility_name
    clinic_f <- f[which(f$long >= fac$longitude_low & f$long <= fac$longitude_high
                        & f$lat <= fac$latitude_high & f$lat >= fac$latitude_low),]
    # filter obs in the facility radius
    
    num_clinic <- NROW(clinic_f)
    # include those with neither penta1 nor penta3?
    facilities[which(facilities$facility_name == name),]$TEHSIL <- clinic_f[1,]$TEHSIL
    # clinic_f includes different tehsils; problem in lat&long adjustment?
    
    facilities[which(facilities$facility_name == name),]$in_clinic <- facilities[(facilities$facility_name == name),]$in_clinic + num_clinic
    if(is.na(facilities[which(facilities$facility_name == name),]$TEHSIL)){
      next
    }
    tehs <- facilities[which(facilities$facility_name == name),]$TEHSIL
    
    in_clinics <- in_clinics + NROW(clinic_f)
    clinic_f$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(clinic_f$Vaccination)),1,0)
    clinic_f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(clinic_f$Vaccination)),1,0)
    # whether this obs got penta vacc
    
    instance.penta1 <- sum(clinic_f$has_penta1)
    instance.penta3<- sum(clinic_f$has_penta3)
    tehsils[(tehsils$TEHSIL == tehs),]$penta1_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta1_in_clinic + instance.penta1
    tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
    facilities[(facilities$facility_name == name),]$penta1 <- facilities[(facilities$facility_name == name),]$penta1 + instance.penta1
    facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
  }
  
  # out clinic
  out_clinics <- out_clinics + (NROW(f) - in_clinics)
  for(k in 1:NROW(tehsils)){
    t <- tehsils$TEHSIL[k]
    if(is.na(t)){
      next
    }
    ftable <- f[(f$TEHSIL == t),]
    fnum <- NROW(ftable)
    ftable$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(ftable$Vaccination)),1,0)
    ftable$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(ftable$Vaccination)),1,0)
    fpenta1 <- sum(ftable$has_penta1)
    fpenta3<- sum(ftable$has_penta3)
    penta1_out <- fpenta1 - tehsils[(tehsils$TEHSIL == t),]$penta1_in_clinic
    penta3_out <- fpenta3 - tehsils[(tehsils$TEHSIL == t),]$penta3_in_clinic
    tehsils[(tehsils$TEHSIL == t),]$penta1_out_clinic <- tehsils[(tehsils$TEHSIL == t),]$penta1_out_clinic + penta1_out
    tehsils[which(tehsils$TEHSIL == t),]$penta3_out_clinic <- tehsils[(tehsils$TEHSIL == t),]$penta3_out_clinic + penta3_out
  }
}


tehsils$penta1_ratio <-tehsils$penta1_in_clinic/(tehsils$penta1_out_clinic + tehsils$penta1_in_clinic)
tehsils$penta3_ratio <-tehsils$penta3_in_clinic/(tehsils$penta3_out_clinic + tehsils$penta3_in_clinic)

# Calculate Clinic to Outreach Penta3 Vacc Ratio

tehsils$OutreachClinicRatio <- tehsils$penta3_out /
  tehsils$penta3_in_clinic

# Calculate Total Clinic and Outreach Vacc Coverages

tehsils$TotalOutreachCoverage <- tehsils$penta3_out / tehsils$child_population
tehsils$TotalClinicsCoverage <- tehsils$penta3_in_clinic / tehsils$child_population

# Get Clinics and Outreach Penta Vaccination data for each Punjab District

districts$penta1_out_clinic <- 0
districts$penta3_out_clinic <- 0
districts$penta1_in_clinic <- 0
districts$penta3_in_clinic <- 0

for(i in 1:NROW(tehsils)){
  districts[which(tehsils$DISTRICT[i] == district$DISTRICT),]$penta1_out_clinic <-
    districts[which(tehsils$DISTRICT[i] == district$DISTRICT),]$penta1_out_clinic +
    tehsils$penta1_out_clinic[i]
  districts[which(tehsils$DISTRICT[i] == district$DISTRICT),]$penta3_out_clinic <-
    districts[which(tehsils$DISTRICT[i] == district$DISTRICT),]$penta3_out_clinic +
    tehsils$penta3_out_clinic[i]
  districts[which(tehsils$DISTRICT[i] == district$DISTRICT),]$penta1_in_clinic <-
    districts[which(tehsils$DISTRICT[i] == district$DISTRICT),]$penta1_in_clinic +
    tehsils$penta1_in_clinic[i]
  districts[which(tehsils$DISTRICT[i] == district$DISTRICT),]$penta1_in_clinic <-
    districts[which(tehsils$DISTRICT[i] == district$DISTRICT),]$penta1_in_clinic +
    tehsils$penta3_in_clinic[i]
}

# Calculate Clinic to Outreach Penta3 Vacc Ratio for Districts

districts$OutreachClinicRatio <- districts$penta3_out /
  districts$penta3_in_clinic

# Calculate Total Clinic and Outreach Vacc Coverages for Districts

districts$TotalOutreachCoverage <- districts$penta3_out / districts$child_population
districts$TotalClinicsCoverage <- districts$penta3_in_clinic / districts$child_population

districts$penta1_ratio <-districts$penta1_in_clinic/(districts$penta1_out_clinic + districts$penta1_in_clinic)
districts$penta3_ratio <-districts$penta3_in_clinic/(districts$penta3_out_clinic + districts$penta3_in_clinic)





####### NOTE THE BELOW CODE DOES NOT NEED TO BE RUN BUT MERGES TEHSIL AND DISTRICT DATA WITH TOTAL PENTA VACC #####

# Clean the EPI files to make them joinable with geo data

clean_df <- function(fl){
  file1 <- read.csv(fl)
  file1 <- file1[,-10]
  file1 <- file1[!duplicated(file1), ]
  file1 <- file1 %>%
    separate(location, into = c('lat', 'long'), sep=",")
  file1$has_tehsil <- 1
  file1[which(file1$town_name == ""),]$has_tehsil <- 0
  no_town <- file1[which(file1$has_tehsil == 0),]
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
  use_tehsil <- file1[which(file1$has_tehsil == 1),]
  new_use_coords <- f_pts[,c(3,4)]
  new_use_tehs <- use_tehsil[,c(2,3,7)]
  new_use_coords <- cbind(new_use_coords,use_coords$Vaccination)
  colnames(new_use_tehs)[colnames(new_use_tehs)=="town_name"] <- "TEHSIL"
  colnames(new_use_tehs)[colnames(new_use_tehs)=="district_name"] <- "DISTRICT"
  colnames(new_use_coords)[colnames(new_use_coords) == "use_coords$Vaccination"] <- "Vaccination"
  colnames(new_use_tehs)[colnames(new_use_tehs) == "daily_reg_no"] <- "Vaccination"
  vaccs_data <- rbind(new_use_coords,new_use_tehs)
  vaccs_data[!is.na(vaccs_data$TEHSIL),]
}

### Extract Penta Vacc Stats for Tehsils and Districts

# Note that Penta1 = Pentavalent Vaccine Dose 1; Penta2 = Pentavalent Dose 2; Penta3 = Pentavalent Dose 3
marker <- 0
first_run_flag <- 1

# For Each EPI File, Join Tehsil and District Level Data to Pentavalent Vaccination Data
# (NOTE: This may take awhile as these EPI files are extremely large)

for(i in 1:length(epi_files[1:45])){
  f <- epi_files[i]
  if(first_run_flag == 1){
    tehsils$vaccinations <- 0
    tehsils$penta1<-0
    tehsils$penta2<-0
    tehsils$penta3<-0
    
    districts$vaccinations <- 0
    districts$penta1<-0
    districts$penta2<-0
    districts$penta3<-0
    first_run_flag <- 0
  }
  temp <- clean_df(f)
  temp$Vaccination <- tolower(temp$Vaccination)
  temp$TEHSIL <- sapply(temp$TEHSIL,solve_name)
  temp$DISTRICT <- sapply(temp$TEHSIL,solve_district_name)
  tehsils_vacc_table = table(temp$TEHSIL)
  districts_vacc_table = table(temp$DISTRICT)
  
  for(j in 1:length(tehsils_vacc_table)){
    name <- as.character(names(tehsils_vacc_table[j]))
    vdf <- temp[which(temp$TEHSIL == name),]
    vdf$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(vdf$Vaccination)),1,0)
    vdf$has_penta2 <- ifelse(grepl("pentavalent-2", tolower(vdf$Vaccination)),1,0)
    vdf$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(vdf$Vaccination)),1,0)
    instance.penta1 <- sum(vdf$has_penta1)
    instance.penta2 <- sum(vdf$has_penta2)
    instance.penta3 <- sum(vdf$has_penta3)
    ex <- tehsils[which(tehsils$TEHSIL == name),]
    tehsils[which(tehsils$TEHSIL == name),]$penta1 <- as.numeric(ex$penta1) + instance.penta1
    tehsils[which(tehsils$TEHSIL == name),]$penta2 <- as.numeric(ex$penta2) + instance.penta2
    tehsils[which(tehsils$TEHSIL == name),]$penta3 <- as.numeric(ex$penta3) + instance.penta3
  }
  for(k in 1:length(districts_vacc_table)){
    name <- as.character(names(districts_vacc_table[k]))
    vdf2 <- temp[which(temp$DISTRICT == name),]
    vdf2$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(vdf2$Vaccination)),1,0)
    vdf2$has_penta2 <- ifelse(grepl("pentavalent-2", tolower(vdf2$Vaccination)),1,0)
    vdf2$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(vdf2$Vaccination)),1,0)
    instance.penta1 <- sum(vdf$has_penta1)
    instance.penta2 <- sum(vdf$has_penta2)
    instance.penta3 <- sum(vdf$has_penta3)
    ex <- districts[which(districts$DISTRICT == name),]
    districts[which(districts$DISTRICT == name),]$penta1 <- as.numeric(ex$penta1) + instance.penta1
    districts[which(districts$DISTRICT == name),]$penta2 <- as.numeric(ex$penta2) + instance.penta2
    districts[which(districts$DISTRICT == name),]$penta3 <- as.numeric(ex$penta3) + instance.penta3
  }
}

# Get Pentavalent Vaccination Coverage Data by Dividing N Pentavalent Vaccintion Dosages by Child Pop Size

tehsils$pentavalent1_vacc_per_capita <- tehsils$penta1 / tehsils$child_population
tehsils$pentavalent2_vacc_per_capita <- tehsils$penta2 / tehsils$child_population
tehsils$pentavalent3_vacc_per_capita <- tehsils$penta3 / tehsils$child_population

districts$pentavalent1_vacc_per_capita <- districts$penta1 / districts$child_population
districts$pentavalent2_vacc_per_capita <- districts$penta2 / districts$child_population
districts$pentavalent3_vacc_per_capita <- districts$penta3 / districts$child_population


##########  this works ----

### clear up
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


### tehsil ----

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




### uc

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
     
      num_teh <- length(unique(clinic_f$UC))   # for different ucs in the clinic range
      for (t in 1:num_teh) {
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

### merge outcome and covariates
ucs.merge <- ucs[, c(2:4, 21,22)]  # outcome variable
ucs_covar <- read.csv("results/ucs_covariates.csv")  # covariates
ucs.complete <- merge(ucs_covar, ucs.merge, by = c("UC", "DISTRICT","TEHSIL"), all.x = T)  # merged complete df
