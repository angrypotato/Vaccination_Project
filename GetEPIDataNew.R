
## Purpose: To Extract Vaccination Data and Map To Tehsil and UC Level Geographic Data 
## Note that these Files are Large and Data Manipulation May Take a While

# Preparation ----

## Call Source File for Required Libraries and Functions

source(file='PreRunNew.r')
# tehsils <- read.csv("results/tehsils_mics.csv")
# districts <- read.csv("results/districts_mics.csv")
# ucs <- read.csv("results/ucs_covariates.csv")

## Read in Past EPI Level Extract Files to Get Vaccination Data and Combine Them

epi_files_17 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2017/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_17 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2017/Non-EPI-Updated", full.names = T)
epi_files_18 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2018/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_18 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2018/Non-EPI-Updated", full.names = T)
epi_files_19 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2019/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_19 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2019/Non-EPI-Updated", full.names = T)

epi_files <- c(epi_files_17,non_epi_files_17,epi_files_18,non_epi_files_18,
               epi_files_19,non_epi_files_19)

## functions ----

### to clean Individual EPI Files to ready them for Joining With Geographic Data
### fl = Single EPI File

clean_df <- function(fl){
  file1 <- read.csv(fl)
  file1 <- file1[,-10]
  file1 <- file1[!duplicated(file1), ]
  file1 <- file1 %>%
    separate(location, into = c('lat', 'long'), sep=",")
  file1$has_tehsil <- 1
  file1[which(file1$town_name == ""),]$has_tehsil <- 0
  file1 <- file1[grepl('penta',file1$Vaccination,ignore.case=TRUE),]
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

### Function to clean Individual EPI Files to ready them for Joining With Geographic Data
### fl = Single EPI File

clean_df2 <- function(fl){
  file1 <- read.csv(fl)
  file1 <- file1[,-10]
  file1 <- file1[!duplicated(file1), ]
  file1 <- file1 %>%
    separate(location, into = c('lat', 'long'), sep=",")
  file1$has_tehsil <- 1
  file1[(file1$town_name == ""),]$has_tehsil <- 0
  file1 <- file1[grepl('penta',file1$Vaccination,ignore.case=TRUE),]
  # file1 <- file1[grepl('penta',file1$Vaccination),]
  
  no_town <- file1[(file1$has_tehsil == 0),]
  # no_town <- file1[which(file1$has_tehsil == 0),]
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
  #print(names(new_use_coords))
  #print(names(new_use_tehs))
  new_use_coords[complete.cases(new_use_coords$TEHSIL),]
}


# Extract Penta Vacc Stats for Tehsils and Districts ----

# Note that Penta1 = Pentavalent Vaccine Dose 1; Penta2 = Pentavalent Dose 2; Penta3 = Pentavalent Dose 3
marker <- 0
first_run_flag <- 1

# For Each EPI File, Join Tehsil and District Level Data to Pentavalent Vaccination Data
# (NOTE: This may take awhile as these EPI files are extremely large)

for(i in 1:length(epi_files)){
  ### epi_files[1:45]? why 45???
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
  
  # tehsil
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
  
  # district
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


# Get Pentavalent Vaccination Coverage Data  ----
## Dividing N Pentavalent Vaccintion Dosages by Child Pop Size

tehsils$pentavalent1_vacc_per_capita <- tehsils$penta1 / tehsils$child_population
tehsils$pentavalent2_vacc_per_capita <- tehsils$penta2 / tehsils$child_population
tehsils$pentavalent3_vacc_per_capita <- tehsils$penta3 / tehsils$child_population

districts$pentavalent1_vacc_per_capita <- districts$penta1 / districts$child_population
districts$pentavalent2_vacc_per_capita <- districts$penta2 / districts$child_population
districts$pentavalent3_vacc_per_capita <- districts$penta3 / districts$child_population


# Read in Clinics Data and Adjust Lat/Longs ----

library(readxl)
facilities <- read_excel("VaccinationStudy/Data/Facilities_location.xls")

facilities$latitude_high <- facilities$latitude + facilities$Latitude_rad
facilities$latitude_low <- facilities$latitude - facilities$Latitude_rad 
facilities$longitude_high <- facilities$longitude + facilities$Long_rad
facilities$longitude_low <- facilities$longitude - facilities$Long_rad

facilities$latitude_high <- facilities$latitude + .001
facilities$latitude_low <- facilities$latitude - .125
facilities$longitude_high <- facilities$longitude + .125
facilities$longitude_low <- facilities$longitude - .125

facilities$latitude_high <- facilities$latitude + .0025
facilities$latitude_low <- facilities$latitude - .0025
facilities$longitude_high <- facilities$longitude + .0025
facilities$longitude_low <- facilities$longitude - .0025

facilities$latitude_high <- facilities$latitude + .001
facilities$latitude_low <- facilities$latitude - .001
facilities$longitude_high <- facilities$longitude + .001
facilities$longitude_low <- facilities$longitude - .001

in_clinics <- 0
out_clinics <- 0 
tehsils$penta1_in_clinic <- 0
tehsils$penta3_in_clinic <- 0
tehsils$penta1_out_clinic <- 0
tehsils$penta3_out_clinic <- 0

districts$penta1_in_clinic <- 0
districts$penta3_in_clinic <- 0
districts$penta1_out_clinic <- 0
districts$penta3_out_clinic <- 0

facilities$penta1 <- 0
facilities$penta3 <- 0
facilities$TEHSIL <- ""
facilities$in_clinic <- 0
facilities$out_clinic <- 0

## tehsils ----

for(file in 1:length(epi_files)){
  f <- clean_df2(epi_files[file])
  f$Vaccination <- tolower(f$Vaccination)
  f$TEHSIL <- toupper(f$TEHSIL)
  for(fa in 1:NROW(facilities)){
    fac <- facilities[fa,]
    name <- fac$facility_name
    clinic_f <- f[which(f$long >= fac$longitude_low & f$long <= fac$longitude_high
                        & f$lat <= fac$latitude_high & f$lat >= fac$latitude_low),]
    num_clinic <- NROW(clinic_f)
    facilities[which(facilities$facility_name == name),]$TEHSIL <- clinic_f[1,]$TEHSIL
    facilities[which(facilities$facility_name == name),]$in_clinic <- facilities[(facilities$facility_name == name),]$in_clinic + num_clinic
    if(is.na(facilities[which(facilities$facility_name == name),]$TEHSIL)){
      next
    }
    tehs <- facilities[which(facilities$facility_name == name),]$TEHSIL
    
    in_clinics <- in_clinics + NROW(clinic_f)
    clinic_f$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(clinic_f$Vaccination)),1,0)
    clinic_f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(clinic_f$Vaccination)),1,0)
    instance.penta1 <- sum(clinic_f$has_penta1)
    instance.penta3<- sum(clinic_f$has_penta3)
    tehsils[(tehsils$TEHSIL == tehs),]$penta1_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta1_in_clinic + sum(clinic_f$has_penta1)
    tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic + sum(clinic_f$has_penta3)
    facilities[(facilities$facility_name == name),]$penta1 <- facilities[(facilities$facility_name == name),]$penta1 + instance.penta1
    facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
  }
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



districts$penta1_out_clinic <- 0
districts$penta3_out_clinic <- 0
districts$penta1_in_clinic <- 0
districts$penta3_in_clinic <- 0

## districts ----

for(i in 1:NROW(tehsils)){
  districts[which(tehsils$DISTRICT[i] == districts$DISTRICT),]$penta1_out_clinic <-
    districts[which(tehsils$DISTRICT[i] == districts$DISTRICT),]$penta1_out_clinic + 
    tehsils$penta1_out_clinic[i]
  districts[which(tehsils$DISTRICT[i] == districts$DISTRICT),]$penta3_out_clinic <-
    districts[which(tehsils$DISTRICT[i] == districts$DISTRICT),]$penta3_out_clinic + 
    tehsils$penta3_out_clinic[i]
  districts[which(tehsils$DISTRICT[i] == districts$DISTRICT),]$penta1_in_clinic <-
    districts[which(tehsils$DISTRICT[i] == districts$DISTRICT),]$penta1_in_clinic + 
    tehsils$penta1_in_clinic[i]
  districts[which(tehsils$DISTRICT[i] == districts$DISTRICT),]$penta1_in_clinic <-
    districts[which(tehsils$DISTRICT[i] == districts$DISTRICT),]$penta1_in_clinic + 
    tehsils$penta3_in_clinic[i]
}

# write.csv(tehsils, "D:\\files\\chunara lab\\vaccination project\\VaccinationProject\\tehsils_epi.csv")
# write.csv(districts, "D:\\files\\chunara lab\\vaccination project\\VaccinationProject\\districts_epi.csv")

### negative results?
### 6.10

districts$penta1_ratio <-tehsils$penta1_in_clinic/(tehsils$penta1_out_clinic + tehsils$penta1_in_clinic)
districts$penta3_ratio <-tehsils$penta3_in_clinic/(tehsils$penta3_out_clinic + tehsils$penta3_in_clinic)

tehsils$Rai
library(RANN)
closest <- nn2(data=xy, k=2)[[1]]


for(facility in facilities){
  
}



# facility lat and long adjustments ----

facilities <- read.csv('/Users/allanporter/Downloads/Covariate Sources - Google Sheets_files/Facilities_location.csv')

facilities$latitude_high <- facilities$latitude + facilities$Latitude_rad
facilities$latitude_low <- facilities$latitude - facilities$Latitude_rad 
facilities$longitude_high <- facilities$longitude + facilities$Long_rad
facilities$longitude_low <- facilities$longitude - facilities$Long_rad

facilities$latitude_high <- facilities$latitude + .001
facilities$latitude_low <- facilities$latitude - .125
facilities$longitude_high <- facilities$longitude + .125
facilities$longitude_low <- facilities$longitude - .125

facilities$latitude_high <- facilities$latitude + .0025
facilities$latitude_low <- facilities$latitude - .0025
facilities$longitude_high <- facilities$longitude + .0025
facilities$longitude_low <- facilities$longitude - .0025

facilities$latitude_high <- facilities$latitude + .001
facilities$latitude_low <- facilities$latitude - .001
facilities$longitude_high <- facilities$longitude + .001
facilities$longitude_low <- facilities$longitude - .001

buffer <- c(.0025,.005,.01,.015,.025,.05,.1)
for(b in buffer){
  facilities$latitude_high <- facilities$latitude + .b
  facilities$latitude_low <- facilities$latitude - b
  facilities$longitude_high <- facilities$longitude + b
  facilities$longitude_low <- facilities$longitude - b
  
  new <- add_facilities_to_df()
  test_with_new_feats(new)
  test_with_sig_feats(new)
  test_with_all_feats(new)
  
  
  
  
}



punjab<-coord_info[(coord_info$p_included==1),]


## UC level ----

add_facilities_to_df <- function(){
  
  in_clinics <- 0
  out_clinics <- 0 
  coord_info$penta1_in_clinic <- 0
  coord_info$measles_in_clinic <- 0
  coord_info$penta3_in_clinic <- 0
  coord_info$penta1_out_clinic <- 0
  coord_info$measles_out_clinic <- 0
  coord_info$penta3_out_clinic <- 0
  uc_pop_df$penta1_in_clinic <- 0
  uc_pop_df$penta3_in_clinic <- 0
  uc_pop_df$penta1_out_clinic <- 0
  uc_pop_df$penta3_out_clinic <- 0
  facilities$penta1 <- 0
  facilities$penta3 <- 0
  facilities$measles <- 0
  facilities$bcg <- 0
  facilities$pcv <- 0
  facilities$TEHSIL <- ""
  facilities$UC <- ""
  facilities$in_clinic <- 0
  facilities$out_clinic <- 0
  for(file in 1:length(files_17[1])){
    if(i == 5 || i == 12 || i == 10 || i==19 || i == 35 || i==23 || i ==24 || i==25 || i==1 || i==2){
      next
    }
    #print("HERE")
    print(i)
    f <- clean_df2(files_17[file])
    to_match <- c("pneum","pneom","pcv","pneomococcal")
    f$Vaccination <- tolower(f$Vaccination)
    f$TEHSIL <- toupper(f$TEHSIL)
    f$UC <- toupper(f$uc_coords)
    for(fa in 1:NROW(facilities)){
      #for(fa in 1:1){
      fac <- facilities[fa,]
      name <- fac$facility_name
      #print(names(f))
      #f <- f[(f$DISTRICT == toupper(fac$District)),]
      clinic_f <- f[(f$long >= fac$longitude_low & f$long <= fac$longitude_high
                     & f$lat <= fac$latitude_high & f$lat >= fac$latitude_low),]
      num_clinic <- NROW(clinic_f)
      print(num_clinic)
      print(NROW(f))
      print(fa)
      #print(num_clinic)
      #print(facilities[(facilities$facility_name == name),])
      facilities[(facilities$facility_name == name),]$TEHSIL <- clinic_f[1,]$TEHSIL
      facilities[(facilities$facility_name == name),]$UC <- clinic_f[1,]$UC
      
      #print("Horchata")
      facilities[(facilities$facility_name == name),]$in_clinic <- facilities[(facilities$facility_name == name),]$in_clinic + num_clinic
      #print("Hor")
      #facilities[(facilities$facility_name == name),]$TEHSIL <- clinic_f$TEHSIL[1]
      if(is.na(facilities[(facilities$facility_name == name),]$TEHSIL)){
        print("Anot")
        next
      }
      if(is.na(facilities[(facilities$facility_name == name),]$UC)){
        print("Anot")
        next
      }
      tehs <- facilities[(facilities$facility_name == name),]$TEHSIL
      ucs <- facilities[(facilities$facility_name == name),]$UC
      print(tehs)
      in_clinics <- in_clinics + NROW(clinic_f)
      clinic_f$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(clinic_f$Vaccination)),1,0)
      clinic_f$has_bcg <- ifelse(grepl("bcg", tolower(clinic_f$Vaccination)),1,0)
      clinic_f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(clinic_f$Vaccination)),1,0)
      clinic_f$has_pcv <- ifelse(grepl("pcv", tolower(clinic_f$Vaccination)),1,0)
      clinic_f$has_measles <- ifelse(grepl("measles", tolower(clinic_f$Vaccination)),1,0)
      clinic_f$has_pcv <- ifelse(grepl(paste(to_match,collapse="|"),tolower(clinic_f$Vaccination)),1,0)
      instance.measles <- sum(clinic_f$has_measles)
      instance.pcv <- sum(clinic_f$has_pcv)
      instance.penta1 <- sum(clinic_f$has_penta1)
      instance.penta3<- sum(clinic_f$has_penta3)
      instance.bcg <- sum(clinic_f$has_bcg)
      uc_pop_df[(uc_pop_df$UC == ucs),]$penta1_in_clinic <- uc_pop_df[(uc_pop_df$UC == ucs),]$penta1_in_clinic + sum(clinic_f$has_penta1)
      uc_pop_df[(uc_pop_df$UC == ucs),]$penta3_in_clinic <- uc_pop_df[(uc_pop_df$UC == ucs),]$penta3_in_clinic + sum(clinic_f$has_penta3)
      coord_info[(coord_info$TEHSIL == tehs),]$penta3_in_clinic <- coord_info[(coord_info$TEHSIL == tehs),]$penta3_in_clinic + sum(clinic_f$has_penta3)
      coord_info[(coord_info$TEHSIL == tehs),]$measles_in_clinic <- coord_info[(coord_info$TEHSIL == tehs),]$measles_in_clinic + sum(clinic_f$has_measles)
      coord_info[(coord_info$TEHSIL == tehs),]$penta1_in_clinic <- coord_info[(coord_info$TEHSIL == tehs),]$penta1_in_clinic + sum(clinic_f$has_penta1)
      coord_info[(coord_info$TEHSIL == tehs),]$penta3_in_clinic <- coord_info[(coord_info$TEHSIL == tehs),]$penta3_in_clinic + sum(clinic_f$has_penta3)
      coord_info[(coord_info$TEHSIL == tehs),]$measles_in_clinic <- coord_info[(coord_info$TEHSIL == tehs),]$measles_in_clinic + sum(clinic_f$has_measles)
      facilities[(facilities$facility_name == name),]$penta1 <- facilities[(facilities$facility_name == name),]$penta1 + instance.penta1
      facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      facilities[(facilities$facility_name == name),]$bcg <- facilities[(facilities$facility_name == name),]$bcg + instance.bcg
      facilities[(facilities$facility_name == name),]$pcv <- facilities[(facilities$facility_name == name),]$pcv + instance.pcv
      facilities[(facilities$facility_name == name),]$measles <- facilities[(facilities$facility_name == name),]$measles + instance.measles
    }
    out_clinics <- out_clinics + (NROW(f) - in_clinics)
    for(k in 1:NROW(punjab)){
      t <- coord_info[(coord_info$p_included == 1),]$TEHSIL[k]
      # <- coord_info[(coord_info$p_included == 1),]$TEHSIL[k]
      
      print(t)
      if(is.na(t)){
        next
      }
      ftable <- f[(f$TEHSIL == t),]
      fnum <- NROW(ftable)
      ftable$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(ftable$Vaccination)),1,0)
      ftable$has_measles <- ifelse(grepl("measles", tolower(ftable$Vaccination)),1,0)
      ftable$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(ftable$Vaccination)),1,0)
      fmeasles <- sum(ftable$has_measles)
      fpenta1 <- sum(ftable$has_penta1)
      fpenta3<- sum(ftable$has_penta3)
      measles_out <- fmeasles - coord_info[(coord_info$TEHSIL == t),]$measles_in_clinic
      penta1_out <- fpenta1 - coord_info[(coord_info$TEHSIL == t),]$penta1_in_clinic
      penta3_out <- fpenta3 - coord_info[(coord_info$TEHSIL == t),]$penta3_in_clinic
      coord_info[(coord_info$TEHSIL == t),]$penta1_out_clinic <- coord_info[(coord_info$TEHSIL == t),]$penta1_out_clinic + penta1_out
      coord_info[(coord_info$TEHSIL == t),]$penta3_out_clinic <- coord_info[(coord_info$TEHSIL == t),]$penta3_out_clinic + penta3_out
      coord_info[(coord_info$TEHSIL == t),]$measles_out_clinic <- coord_info[(coord_info$TEHSIL == t),]$measles_out_clinic  + measles_out
    }
    for(k in 1:NROW(uc_pop_df)){
      t <- uc_pop_df$UC[k]
      # <- coord_info[(coord_info$p_included == 1),]$TEHSIL[k]
      
      print(t)
      if(is.na(t)){
        next
      }
      ftable <- f[(f$UC == t),]
      fnum <- NROW(ftable)
      ftable$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(ftable$Vaccination)),1,0)
      #ftable$has_measles <- ifelse(grepl("measles", tolower(ftable$Vaccination)),1,0)
      ftable$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(ftable$Vaccination)),1,0)
      #fmeasles <- sum(ftable$has_measles)
      fpenta1 <- sum(ftable$has_penta1)
      fpenta3<- sum(ftable$has_penta3)
      #measles_out <- fmeasles - coord_info[(coord_info$TEHSIL == t),]$measles_in_clinic
      penta1_out <- fpenta1 - coord_info[(coord_info$TEHSIL == t),]$penta1_in_clinic
      penta3_out <- fpenta3 - coord_info[(coord_info$TEHSIL == t),]$penta3_in_clinic
      uc_pop_df[(uc_pop_df$UC == t),]$penta1_out_clinic <- uc_pop_df[(uc_pop_df$UC == t),]$penta1_out_clinic + penta1_out
      uc_pop_df[(uc_pop_df$UC == t),]$penta3_out_clinic <- uc_pop_df[(uc_pop_df$UC == t),]$penta3_out_clinic + penta3_out
    }
  }
  coord_info$penta1_in_clinic_ratio <- coord_info$penta1_in_clinic / coord_info$penta1
  coord_info$penta3_in_clinic_ratio <- coord_info$penta3_in_clinic / coord_info$penta3
  uc_pop_df$penta1_in_clinic_ratio <- coord_info$penta1_in_clinic / coord_info$penta1
  uc_pop_df$penta3_in_clinic_ratio <- coord_info$penta3_in_clinic / coord_info$penta3
  
  
  coord_info
}


uc_pops <- read.csv('/Users/allanporter/Desktop/School Folder/uc_pop_df.csv')
coord_info2 <- coord_info[-c(98,113),]
punjab_covars$penta1_in_clinic_ratio <- coord_info2$penta1_in_clinic_ratio
punjab_covars$penta3_in_clinic_ratio <- coord_info2$penta3_in_clinic_ratio
