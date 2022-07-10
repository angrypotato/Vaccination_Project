# Prep


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


epi_files_17 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2017/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_17 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2017/Non-EPI-Updated", full.names = T)
epi_files_18 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2018/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_18 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2018/Non-EPI-Updated", full.names = T)
epi_files_19 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2019/EPI-Updated", pattern = "*.csv", full.names = T)
non_epi_files_19 <- list.files(path = "VaccinationStudy/Data/E-Vaccs Data/2019/Non-EPI-Updated", full.names = T)

epi_files <- c(epi_files_17,non_epi_files_17,epi_files_18,non_epi_files_18,
               epi_files_19,non_epi_files_19)


facilities <- readxl::read_xls('VaccinationStudy/Data/Facilities_location.xls')

facilities$latitude_high <- facilities$latitude + .0015
facilities$latitude_low <- facilities$latitude - .0015
facilities$longitude_high <- facilities$longitude + .0015
facilities$longitude_low <- facilities$longitude - .0015


uc_shp <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs <- readOGR("VaccinationStudy/Data/Adminbdy Shapefile/Union_Council.shp")
ucs@data$id <- rownames(ucs@data)
ucs <- data.frame(ucs)
ucs <- ucs[which(ucs$PROVINCE == 'Punjab'),] %>%
  mutate(UC = toupper(UC))


in_clinics <- 0
out_clinics <- 0
tot.instance.penta3 <- 0

ucs$penta3_in_clinic <- 0
ucs$penta3_out_clinic <- 0

facilities$penta3 <- 0
facilities$TEHSIL <- ""
facilities$in_clinic <- 0
facilities$out_clinic <- 0

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

write.csv(ucs, "uc_vacc_new.csv")