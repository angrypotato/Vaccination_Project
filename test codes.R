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

coordinates(file2)<- ~x +y  #
proj4string(file2) <- proj4string(uc_shp)  ##
pts2 <- over(file2,uc_shp)    ##

pts2_test <- pts2[,1:4] %>%
  na.omit() %>%
  filter(PROVINCE == "Punjab")

pts2_test2 <- pts2[which(pts2$PROVINCE == "Punjab"), ]


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



# get epi data ----

epi_origin <- read.csv("VaccinationStudy/Data/E-Vaccs Data/2017/Non-EPI-Updated/nonepi_2017_01.csv")
epi_test <- clean_df2("VaccinationStudy/Data/E-Vaccs Data/2017/Non-EPI-Updated/nonepi_2017_01.csv")

epi_test$Vaccination <- tolower(epi_test$Vaccination)
epi_test$TEHSIL <- toupper(epi_test$TEHSIL)

## for each facility -1 ----
for(fa in 1:NROW(facilities)){
  fac <- facilities[fa,]
  name <- fac$facility_name
  clinic_f <- epi_test[which(epi_test$long >= fac$longitude_low & epi_test$long <= fac$longitude_high
                      & epi_test$lat <= fac$latitude_high & epi_test$lat >= fac$latitude_low),]
  # filter obs in the facility radius
  
  num_clinic <- NROW(clinic_f)
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
  # whether this obs got penta1 vacc
  
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


### determine the facility tehsil info

facilities$latitude_high <- facilities$latitude + .0015
facilities$latitude_low <- facilities$latitude - .0015
facilities$longitude_high <- facilities$longitude + .0015
facilities$longitude_low <- facilities$longitude - .0015

facilities$TEHSIL <- NA
test_df <- facilities %>% dplyr::select(facility_name)
#length(epi_files
select = dplyr::select

for(file in 1:3){
  f <- clean_df2(epi_files[file])
  f$TEHSIL <- toupper(f$TEHSIL)
  
  for(fa in 1:NROW(facilities)){
    facilities %>% group_by(facility_name) %>% slice(1) %>% ungroup()
    fac <- facilities[fa,]
    name <- fac$facility_name
    clinic_f <- f[which(f$long >= fac$longitude_low & f$long <= fac$longitude_high
                               & f$lat <= fac$latitude_high & f$lat >= fac$latitude_low),]
    tehsil_tab <- tabyl(clinic_f$TEHSIL)
    
    if (nrow(clinic_f) > 0) {
      fac_tehsil <- tehsil_tab$`clinic_f$TEHSIL`[which(tehsil_tab$n == max(tehsil_tab$n))]
      facilities$TEHSIL[fa] <- fac_tehsil
    }
 
  }
  test_df = cbind(test_df, TEHSIL = facilities$TEHSIL)
}

test_df %>% pivot_longer(-facility_name, names_to = "TEHSIL", values_to = "Epi") %>% 
  dplyr::select(-TEHSIL) %>% group_by(facility_name) %>% 
    count(Epi) %>% slice(which.max(n)) %>% select(-n)



## for each facility -2 ----
### epi_test represents each epi file
### skip the step of assigning tehsil info to each facility

facilities$penta1 <- 0
facilities$in_clinic <- 0
tehsils$penta1_in_clinic <- 0
tehsils$penta3_in_clinic <- 0
tehsils$penta1_out_clinic <- 0
tehsils$penta3_out_clinic <- 0

##########  this works
epi_test$has_penta1 <- 0
epi_test$has_penta3 <- 0

epi_test$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(epi_test$Vaccination)),1,0)
epi_test$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(epi_test$Vaccination)),1,0)

in_clinics<- tehsils$penta3_in_clinic

### in_clinic or outreach
for(fa in 1:NROW(facilities)){
  fac <- facilities[fa,]
  name <- fac$facility_name
  
  # filter obs in the facility radius
  clinic_f <- epi_test[which(epi_test$long >= fac$longitude_low & epi_test$long <= fac$longitude_high
                             & epi_test$lat <= fac$latitude_high & epi_test$lat >= fac$latitude_low),]
  num_clinic <- NROW(clinic_f)

  if (num_clinic >0) {
    facilities[which(facilities$facility_name == name),]$in_clinic <- facilities[(facilities$facility_name == name),]$in_clinic + num_clinic
    num_teh <- length(unique(clinic_f$TEHSIL))
    for (t in 1:num_teh) {
      tehs <- unique(clinic_f$TEHSIL)[t]

      instance.penta1 <- sum(clinic_f[which(clinic_f$TEHSIL == tehs),]$has_penta1)
      instance.penta3 <- sum(clinic_f[which(clinic_f$TEHSIL == tehs),]$has_penta3)
      # instance of each tehsil
      
      tot.instance.penta1 <- tot.instance.penta1 + instance.penta1
      tot.instance.penta3 <- tot.instance.penta3 + instance.penta3
      
      tehsils[(tehsils$TEHSIL == tehs),]$penta1_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta1_in_clinic + instance.penta1
      tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
      facilities[(facilities$facility_name == name),]$penta1 <- facilities[(facilities$facility_name == name),]$penta1 + instance.penta1
      facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      
    }
  }  
}

for(k in 1:NROW(tehsils)) {    ###
  tehs <- tehsils$TEHSIL[k]
  ###
  out_clinics <- out_clinics + (NROW(epi_test[which(epi_test$has_penta3 == 1),]) - in_clinics)
  if(is.na(tehs)){
    next
  }
  ftable <- epi_test[which((epi_test$TEHSIL == tehs) & (epi_test$has_penta3 == 1)),]
  fpenta1 <- sum(ftable$has_penta1)
  fpenta3<- sum(ftable$has_penta3)
 ### in_clinic obs in this file of this tehsil
  penta3_out <- fpenta3 - (tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic - in_clinics[k])

  tehsils[which(tehsils$TEHSIL == tehs),]$penta3_out_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta3_out_clinic + penta3_out
}

########## this works



### out clinic
out_clinics <- out_clinics + (NROW(epi_test[which(epi_test$has_penta3 == 1),]) - in_clinics)
for(k in 1:NROW(tehsils)){
  t <- tehsils$TEHSIL[k]
  if(is.na(t)){
    next
  }
  ftable <- epi_test[which((epi_test$TEHSIL == t) & (epi_test$has_penta3 == 1)),]
  fpenta1 <- sum(ftable$has_penta1)
  fpenta3<- sum(ftable$has_penta3)
  penta1_out <- fpenta1 - ### # in_clinic obs in this file of this tehsil
  ### fpenta1 is the total has_penta1 in each file, while tehsils$penta1_in_clinic is the cumulative value
  penta3_out <- fpenta3 - tot.instance.penta3
  tehsils[(tehsils$TEHSIL == t),]$penta1_out_clinic <- tehsils[(tehsils$TEHSIL == t),]$penta1_out_clinic + penta1_out
  tehsils[which(tehsils$TEHSIL == t),]$penta3_out_clinic <- tehsils[(tehsils$TEHSIL == t),]$penta3_out_clinic + penta3_out
}



### whole loop ----


### clear up
in_clinics <- 0
out_clinics <- 0

in_clinics <- 0
out_clinics <- 0
tehsils$penta1_in_clinic <- 0
tehsils$penta3_in_clinic <- 0
tehsils$penta1_out_clinic <- 0
tehsils$penta3_out_clinic <- 0

facilities$penta1 <- 0
facilities$penta3 <- 0
facilities$TEHSIL <- ""
facilities$in_clinic <- 0
facilities$out_clinic <- 0

tot.instance.penta1 <- 0
tot.instance.penta3 <- 0

### tehsil

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
    
    clinic_f$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(clinic_f$Vaccination)),1,0)
    clinic_f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(clinic_f$Vaccination)),1,0)
    
    num_clinic <- NROW(clinic_f)
    in_clinics <- in_clinics + num_clinic
    
    if (num_clinic >0) {
      facilities[which(facilities$facility_name == name),]$in_clinic <- facilities[(facilities$facility_name == name),]$in_clinic + num_clinic
      
      num_teh <- length(unique(clinic_f$TEHSIL))
      for (t in 1:num_teh) {
        teh_name <- unique(clinic_f$TEHSIL)[t]
        tehs <- teh_name
        
        instance.penta1 <- sum(clinic_f$has_penta1[which(clinic_f$TEHSIL == tehs)])
        instance.penta3<- sum(clinic_f$has_penta3[which(clinic_f$TEHSIL == tehs)])
        tehsils[(tehsils$TEHSIL == tehs),]$penta1_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta1_in_clinic + instance.penta1
        tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic <- tehsils[(tehsils$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta1 <- facilities[(facilities$facility_name == name),]$penta1 + instance.penta1
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
        
      }
    }
    
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





### uc level

for(file in 1:length(epi_files)){
  f <- clean_df2(epi_files[file])    ### function revision
  f$Vaccination <- tolower(f$Vaccination)
  f$UC <- toupper(f$UC)
  
  ## in clinic
  for(fa in 1:NROW(facilities)){
    fac <- facilities[fa,]
    name <- fac$facility_name
    clinic_f <- f[which(f$long >= fac$longitude_low & f$long <= fac$longitude_high
                        & f$lat <= fac$latitude_high & f$lat >= fac$latitude_low),]
    # filter obs in the facility radius
    
    clinic_f$has_penta1 <- ifelse(grepl("pentavalent-1", tolower(clinic_f$Vaccination)),1,0)
    clinic_f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(clinic_f$Vaccination)),1,0)
    
    num_clinic <- NROW(clinic_f)
    in_clinics <- in_clinics + num_clinic
    
    if (num_clinic >0) {
      facilities[which(facilities$facility_name == name),]$in_clinic <- facilities[(facilities$facility_name == name),]$in_clinic + num_clinic
      
      num_teh <- length(unique(clinic_f$TEHSIL))
      for (t in 1:num_teh) {
        uc_name <- unique(clinic_f$UC)[t]
        
        instance.penta1 <- sum(clinic_f$has_penta1[which(clinic_f$TEHSIL == uc_name)])
        instance.penta3<- sum(clinic_f$has_penta3[which(clinic_f$TEHSIL == uc_name)])
        tehsils[(tehsils$TEHSIL == uc_name),]$penta1_in_clinic <- tehsils[(tehsils$TEHSIL == uc_name),]$penta1_in_clinic + instance.penta1
        tehsils[(tehsils$TEHSIL == uc_name),]$penta3_in_clinic <- tehsils[(tehsils$TEHSIL == uc_name),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta1 <- facilities[(facilities$facility_name == name),]$penta1 + instance.penta1
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
        
      }
    }
    
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





# outcome by year ---- 
## 2017
epi_17 <- c(epi_files_17,non_epi_files_17)
test <- tehsils

for(file in 1:length(epi_17)){
  f <- clean_df(epi_files[file])
  f$Vaccination <- tolower(f$Vaccination)
  
  f$has_penta3 <- 0
  f$has_penta3 <- ifelse(grepl("pentavalent-3", tolower(f$Vaccination)),1,0)
  tot.instance.penta3 <- tot.instance.penta3 + sum(f$has_penta3)  # global index
  
  in_clinics <- test$penta3_in_clinic   # local index recording data before running this file
  
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
        
        test[(test$TEHSIL == tehs),]$penta3_in_clinic <- test[(test$TEHSIL == tehs),]$penta3_in_clinic + instance.penta3
        facilities[(facilities$facility_name == name),]$penta3 <- facilities[(facilities$facility_name == name),]$penta3 + instance.penta3
      }
    }  
  }
  
  # outreach
  for(k in 1:NROW(test)) { 
    tehs <- test$TEHSIL[k]
    if(is.na(tehs)){
      next
    }
    
    ftable <- f[which((f$TEHSIL == tehs) & (f$has_penta3 == 1)),]
    fpenta3<- sum(ftable$has_penta3)
    ### in_clinic obs in this file of this tehsil
    penta3_out <- fpenta3 - (test[(test$TEHSIL == tehs),]$penta3_in_clinic - in_clinics[k])
    
    test[which(test$TEHSIL == tehs),]$penta3_out_clinic <- test[(test$TEHSIL == tehs),]$penta3_out_clinic + penta3_out
  }
  print(file)
} 





# model performance ---- 

adj.r2 <- function(r, n, p) {
  1 - (1-r)*(n-1)/(n-p-1)
}


## clinic ----

clinic.step <- gbm.step(
  data=pentaTrain, 
  gbm.x = c(1:3,5:10,12,16,18,19),   # selected features 
  gbm.y = 20,
  family = "gaussian",
  tree.complexity = 2,
  learning.rate = 0.005,
  bag.fraction = 0.5,
  cv_folds = 10,
)

gbm_pred = predict(clinic.step,pentaTest)
rmse(pentaTest[,20],gbm_pred)
R2(pentaTest[,20],gbm_pred)
mae(pentaTest[,20],gbm_pred)

gbm_cfs <- summary(clinic.step)
gbm_cfs <- cbind(data.frame(gbm_cfs[,1]),data.frame(gbm_cfs[,2]))
names(gbm_cfs) <- c("Feature","Rel.Influence")
xtable(data.frame(gbm_cfs))



set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "child_population"=rep(0, 1000), "population_density"=rep(0, 1000), 
                    "radio"=rep(0, 1000), "electricity"=rep(0, 1000),"television"=rep(0, 1000), "mobile_phone"=rep(0, 1000),"urban_to_rural"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalClinicsCoverage
  x <- data.matrix(sample_d[, c(1:3,5,7:12,18)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1:3,5,7:12,18)]))
  rmse <- rmse(pentaTest[,20],preds)
  r2 <- R2(pentaTest[,20],preds)
  mae <- MAE(pentaTest[,20],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

std_mean <- function(x) sd(x)/sqrt(length(x))

coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)), "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),"television"=c(mean(coefs$television), std_mean(coefs$television)), 
                         "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),"urban_to_rural"=c(mean(coefs$urban_to_rural), std_mean(coefs$urban_to_rural)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))






#####
gam.form <- as.formula(TotalClinicsCoverage ~ s(elevation, k=5) + s(poverty, k=5) +  s(distance_to_cities, k=5) +
                         s(Population, k=5) + s(child_population, k=5) + s(population_density, k=5) + 
                         s(radio, k=5)   + s(mothers_age, k=5) +
                         s(urban_to_rural, k=5))

gam.mod <- gam(gam.form, data = pentaTrain, method = "REML")  

gam_preds <- predict(gam.mod, pentaTest)

rmse(pentaTest[,20],gam_preds)
R2(pentaTest[,20],gam_preds)
MAE(pentaTest[,20],gam_preds)

clinic_gam_summary <- summary(gam.mod$finalModel)
clinic_gam_cfs <- -log10(as.data.frame(summary(gam.mod)$s.table)['p-value'])
xtable(data.frame(clinic_gam_cfs))


### ridge ----

y <- pentaTrain$TotalClinicsCoverage
x <- data.matrix(pentaTrain[, c(1:3,5,7:12,18)])

ridge_model <- cv.glmnet(x, y, alpha = 0)

best_lambda <- ridge_model$lambda.min

ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
ridge_outcome <- coef(ridge_best_model)
View(data.frame(ridge_outcome@Dimnames[[1]], ridge_outcome@x))
View(data.frame(ridge_outcome@Dimnames[[1]], abs(ridge_outcome@x)))


clinic_lasso_preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1:2,5,7:9,11,18)]))
rmse(pentaTest[,20],clinic_lasso_preds)
R2(pentaTest[,20],clinic_lasso_preds)
MAE(pentaTest[,20],clinic_lasso_preds)


#### bootstrap for SE ----

set.seed(0)

coefs <- data.frame("Intercept"= rep(0, 1000), "fertility"=rep(0, 1000), "elevation"=rep(0, 1000), "poverty"=rep(0, 1000), "distance_to_cities"=rep(0, 1000),
                    "child_population"=rep(0, 1000), "population_density"=rep(0, 1000), 
                    "radio"=rep(0, 1000), "electricity"=rep(0, 1000),"television"=rep(0, 1000), "mobile_phone"=rep(0, 1000),"urban_to_rural"=rep(0, 1000))

mod_performance <- data.frame("RMSE" = rep(0, 1000), "R2" = rep(0, 1000), "MAE"=rep(0, 1000))

for (i in 1:1000) {
  sample_d = pentaTrain[sample(1:nrow(pentaTrain), nrow(pentaTrain), replace = TRUE), ]
  
  y <- sample_d$TotalClinicsCoverage
  x <- data.matrix(sample_d[, c(1:3,5,7:12,18)])
  
  ridge_model <- cv.glmnet(x, y, alpha = 0)
  
  best_lambda <- ridge_model$lambda.min
  
  ridge_best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  ridge_outcome <- coef(ridge_best_model)
  
  preds <- predict(ridge_best_model, newx=data.matrix(pentaTest[,c(1:3,5,7:12,18)]))
  rmse <- rmse(pentaTest[,20],preds)
  r2 <- R2(pentaTest[,20],preds)
  mae <- MAE(pentaTest[,20],preds)
  
  ## fill in the blank list
  coefs[i,] <- ridge_outcome@x
  mod_performance[i,1] <- rmse
  mod_performance[i,2] <- r2
  mod_performance[i,3] <- mae
}

std_mean <- function(x) sd(x)/sqrt(length(x))

coef_final <- data.frame("Intercept"= c(mean(coefs$Intercept), std_mean(coefs$Intercept)), "fertility"=c(mean(coefs$fertility), std_mean(coefs$fertility)), 
                         "elevation"=c(mean(coefs$elevation), std_mean(coefs$elevation)), "poverty"=c(mean(coefs$poverty), std_mean(coefs$poverty)), 
                         "distance_to_cities"=c(mean(coefs$distance_to_cities), std_mean(coefs$distance_to_cities)), "child_population"=c(mean(coefs$child_population), std_mean(coefs$child_population)), 
                         "population_density"=c(mean(coefs$population_density), std_mean(coefs$population_density)), "radio"=c(mean(coefs$radio), std_mean(coefs$radio)), 
                         "electricity"=c(mean(coefs$electricity), std_mean(coefs$electricity)),"television"=c(mean(coefs$television), std_mean(coefs$television)), 
                         "mobile_phone"=c(mean(coefs$mobile_phone), std_mean(coefs$mobile_phone)),"urban_to_rural"=c(mean(coefs$urban_to_rural), std_mean(coefs$urban_to_rural)))
data.frame("RMSE" = mean(mod_performance$RMSE), "R2" = mean(mod_performance$R2), "MAE" = mean(mod_performance$MAE))




# maternal edu ----

## repeat feature selection ----

library(Boruta)

tehsils <- read.csv("results/tehsils_complete_7.19.csv")
tehsils.clinic <- tehsils[,c(3:21,24,27)] %>%   # 19 features + last col the outcome
  scale() %>%
  as.data.frame() 

tehsils.clinic <- tehsils.clinic[complete.cases(tehsils.clinic[,-4]), -4]  ### 132 obs  ### 7/21 using this


rfe <- list()
boruta <- list()

for (i in 1:5) {
  set.seed(i)
  data_split = sample.split(tehsils.clinic, SplitRatio = 0.8)
  pentaTrain <- subset(tehsils.clinic, data_split == TRUE)
  
  rfcontrol <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10,repeats=3)
  results <- rfe(pentaTrain[,1:19], pentaTrain[,20],sizes=c(1:19), rfeControl=rfcontrol)
  rfe[[i]] <- predictors(results)  
  
  set.seed(i)
  boruta_output <- Boruta(TotalClinicsCoverage ~ ., data=na.omit(pentaTrain), doTrace=2)  # perform Boruta search
  boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
  boruta[[i]] <- print(boruta_signif)
}


## proportion ----

recode_education <- function(x) {
  switch(as.character(x),
         'Preschool' = 1,
         'Primary' = 2,
         'Middle' = 3,
         'Matric' = 4,
         'Above Matric' = 5,
         'Missing' = NA,
         as.numeric(x)
  )
}

wm_df$school_level <- sapply(wm_df$school_level,recode_education)


include_var <- function(df1,df2){
  
  ### remove missing
  
  if(NROW(df2[which(df2[,"school_level"] == "Missing"),])>0){
    df2[which(df2[,"school_level"] == "Missing"),][,"school_level"] <- NA
  }
  df2 <- df2[complete.cases(df2[,"school_level"]),]
  
  ### calculate proportion
  
  x <- "TEHSIL"
  df3 <- data.frame(df2[,c(x,"school_level")] %>% 
                      group_by(TEHSIL) %>% 
                      summarise(teh_obs = n(),
                                Preschool = sum(school_level == 1)/teh_obs,
                                Primary = sum(school_level == 2)/teh_obs,
                                Middle = sum(school_level == 3)/teh_obs,
                                Matric = sum(school_level == 4)/teh_obs,
                                Above = sum(school_level == 5)/teh_obs)
                    )
  
  df <- merge(df1,df3,by=x, all.x=T)

  df
}

tehsils_test <- include_var(tehsils,wm_df)
write.csv(tehsils_test, "results/maternal_edu.csv")
