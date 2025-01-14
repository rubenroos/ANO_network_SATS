library(sf)
library(tmap)
library(tidyverse)
library(terra)
library(readxl)

#### load data ####
### download from kartkatalogen to P-drive
#url <- "https://nedlasting.miljodirektoratet.no/naturovervaking/naturovervaking_eksport.gdb.zip"
#download(url, dest="P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb.zip", mode="w") 
#unzip ("P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb.zip", 
#       exdir = "P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb")

### upload data from P-drive
## ANO
#st_layers(dsn = "P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb")
#ANO.sp <- st_read("P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb", layer="ANO_Art")
ANO.geo <- st_read("P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb", layer="ANO_SurveyPoint")

## workaround for being able to make ANO.geo derived data frames further down the line into spatial objects
# store CRS
ANO.geo.crs <- st_crs(ANO.geo)
# store coords from SHAPE as separate variables and drop geometry
ANO.geo <- ANO.geo %>%
  mutate(lat = st_coordinates(.)[,1],
         long = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

#write.table(ANO.geo, file='C:/Users/joachim.topper/OneDrive - NINA/work/R projects/github/ANO_network_SATS/Data/ANO_geo.txt',quote=FALSE,sep=";",col.names=TRUE,row.names=FALSE,dec=".")
#write.table(ANO.sp, file='C:/Users/joachim.topper/OneDrive - NINA/work/R projects/github/ANO_network_SATS/Data/ANO_sp.txt',quote=FALSE,sep=";",col.names=TRUE,row.names=FALSE,dec=".")

### translation key NiN2_5k to NiN3_20k
NiNtrans <- read_xlsx("C:/Users/joachim.topper/OneDrive - NINA/work/projects/NiN/NiN3/NiN2_5k_NiN3_20k.xlsx", sheet="Typer2")
head(NiNtrans)
summary(NiNtrans)
names(NiNtrans)

NiNtrans <- NiNtrans %>% 
  filter(rødlistevurdering==1) %>%
  filter(NIN2_kartleggingsenhet!='NA' ) %>%
  filter(M020_kode2!='NA' ) %>%
  select(-rødlistevurdering)

NiNtrans <- NiNtrans %>%
  distinct(NIN2_kartleggingsenhet, .keep_all = TRUE)

#### upload maps for vegetation zones and sections
vegzones <- st_read("R:/GeoSpatialData/BiogeographicalRegions/Norway_VegetationZones_Moen/Original/Vector/soner.shp")
vegsections <- st_read("R:/GeoSpatialData/BiogeographicalRegions/Norway_VegetationZones_Moen/Original/Vector/seksjoner.shp",crs=st_crs(vegzones) )  # this file seems to lack a crs, assuming it's the same as for zones (check if map makes sense)

#plot(vegzones['NAVN'])
#plot(vegsections['NAVN'])

### merging classes in vegzones and vegsections
vegzones <- vegzones %>%
  mutate(KLASSE = as.factor(KLASSE)) %>%
  mutate(KLASSE2= case_when(KLASSE %in% c("1","2","3")~"11",
                            KLASSE %in% c("4","5")~"12",
                            KLASSE %in% c("6")~"13"
  )
  )

vegzones <- vegzones %>%
  mutate(NAVN = as.factor(NAVN)) %>%
  mutate(NAVN2= case_when(NAVN %in% c("Nemoral","Boreonemoral","S°rboreal")~"Nemoral_soerboreal",
                          NAVN %in% c("Mellomboreal","Nordboreal")~"Mellom_nordboreal",
                          NAVN %in% c("Alpin")~"Alpin"
  )
  )

#plot(vegzones['KLASSE2'])
#plot(vegzones['NAVN2'])



vegsections <- vegsections %>%
  mutate(KLASSE = as.factor(KLASSE)) %>%
  mutate(KLASSE2= case_when(KLASSE %in% c("1","2")~"11",
                            KLASSE %in% c("3","4","5")~"12",
                            KLASSE %in% c("6")~"13"
  )
  )

vegsections <- vegsections %>%
  mutate(NAVN = as.factor(NAVN)) %>%
  mutate(NAVN2= case_when(NAVN %in% c("O3t-Vintermild","O3-Sterkt ocean")~"O3_O3t",
                          NAVN %in% c("O2-Klart oceani","O1-Svakt oceani","OC-Overgangssek")~"OC_O2",
                          NAVN %in% c("C1-Svakt kontin")~"C1"
  )
  )

#plot(vegsections['KLASSE2'])
#plot(vegsections['NAVN2'])


# combining zones & sections
comb_zones_sections <- st_intersection(vegzones, vegsections)

comb_zones_sections <- comb_zones_sections %>%
  mutate(sone_sek = paste0(NAVN2,"_", NAVN2.1))

#plot(comb_zones_sections['sone_sek'])


#### data handling - ANO data ####
head(ANO.geo)

## fix NiN information on hovedtyper
ANO.geo$hovedtype_rute <- substr(ANO.geo$kartleggingsenhet_1m2,1,3) # take the 3 first characters
ANO.geo$hovedtype_rute <- gsub("-", "", ANO.geo$hovedtype_rute) # remove hyphon
unique(as.factor(ANO.geo$hovedtype_rute))

## replace information in points that have not been mapped at the circle level with NA
unique(ANO.geo$kartleggingsenhet_250m2)
summary(as.factor(ANO.geo$kartleggingsenhet_250m2))
ANO.geo$kartleggingsenhet_250m2 <- factor(ANO.geo$kartleggingsenhet_250m2)
unique(ANO.geo$kartleggingsenhet_250m2)
ANO.geo <- ANO.geo %>%
  mutate(across(kartleggingsenhet_250m2, ~replace(., . %in% c("", NA, "N/A", "ikke_kartlagt"), NA)))
summary(ANO.geo$kartleggingsenhet_250m2)
dim(ANO.geo)
ANO.dat <- ANO.geo %>% filter(!is.na(kartleggingsenhet_250m2) )
dim(ANO.dat)
#summary(as.factor(ANO.dat$kartleggingsenhet_250m2))

#### translating NiN2-codes for kartleggingsenhet_250m2 (1:5000) to NiN3-codes 1:20000
#ANO.dat
ANO.dat <- merge(x=ANO.dat[,c("GlobalID","ano_punkt_id","kartleggingsenhet_250m2","andel_kartleggingsenhet_250m2","lat","long")],
                 y=NiNtrans,
                 by.x="kartleggingsenhet_250m2", by.y="NIN2_kartleggingsenhet", all.x=T)
names(ANO.dat)
summary(as.factor(ANO.dat$M020_kode2))
# transfer info from "kartleggingsenhet_250m2" to "M020_kode2" for NA's in "M020_kode2"
ANO.dat <- ANO.dat %>%   mutate(M020_kode3 = ifelse(is.na(M020_kode2), as.character(kartleggingsenhet_250m2), M020_kode2))

ANO.dat$area <- ANO.dat$andel_kartleggingsenhet_250m2/100*250

# split one-to-several NiN3-types into separate rows
ANO.dat <- ANO.dat %>% separate_longer_delim(c(M020_kode3,andel), delim = "_")
# calculate area share for the rows resulting form the splitting
ANO.dat <- ANO.dat %>% 
  mutate(andel = as.numeric(andel))
ANO.dat[is.na(ANO.dat$andel),'andel'] <- 100

ANO.dat$area <- ANO.dat$area*ANO.dat$andel/100


## adding geometry
ANO.dat <- st_as_sf(ANO.dat,coords=c('lat','long'),crs=ANO.geo.crs, remove=F)

#### area of types in vegzones and vegsections ####

### adding vegzones & vesections to ANO.dat
# change the CRS of ANO.dat to the CRS of the vegzones and -sections maps
ANO.dat <- st_transform(ANO.dat, crs = st_crs(vegzones))


#tm_shape(vegzones) +
#  tm_fill('NAVN2', labels="", title="", legend.show = TRUE) + 
#  tm_borders() +
#  tm_shape(ANO.dat) +
#  tm_dots()

#tm_shape(vegsections) +
#  tm_fill('NAVN2', labels="", title="", legend.show = TRUE) + 
#  tm_borders() +
#  tm_shape(ANO.dat) +
#  tm_dots()


ANO.dat <- st_join(ANO.dat,vegzones[,c("KLASSE2","NAVN2")],join= st_nearest_feature)
names(ANO.dat)[c(13,14)] <- c("zone_class","zone_name")

ANO.dat <- st_join(ANO.dat,vegsections[,c("KLASSE2","NAVN2")],join= st_nearest_feature)
names(ANO.dat)[c(15,16)] <- c("section_class","section_name")

ANO.dat <- st_join(ANO.dat,comb_zones_sections[,c("sone_sek")],join= st_nearest_feature)
names(ANO.dat)[c(17)] <- c("zone_section_name")

### area calculations
# drop geometry from ANO.dat
ANO.dat <- ANO.dat %>% st_drop_geometry()

# calculate area for Norway
M020_Norge_area <- ANO.dat %>%
  group_by(M020_kode3) %>%
  summarize(Norge_area=sum(area,na.rm=T))

# calculate area for zones
M020_zone_area <- ANO.dat %>%
  group_by(M020_kode3,zone_name) %>%
  summarize(zone_area=sum(area,na.rm=T))

# calculate area for sections
M020_section_area <- ANO.dat %>%
  group_by(M020_kode3,section_name) %>%
  summarize(section_area=sum(area,na.rm=T))

# calculate area for zone-section-combinations
M020_zone_section_area <- ANO.dat %>%
  group_by(M020_kode3,zone_section_name) %>%
  summarize(zone_section_area=sum(area,na.rm=T))

## checking if there's any area for points outside zones or sections
ANO.dat[is.na(ANO.dat$zone_name),]
ANO.dat[is.na(ANO.dat$section_name),]
ANO.dat[is.na(ANO.dat$zone_section_name),]

## make wide versions and merge them
M020_zone_area <- pivot_wider(M020_zone_area,names_from = zone_name, values_from = zone_area)
M020_section_area <- pivot_wider(M020_section_area,names_from = section_name, values_from = section_area)
M020_zone_section_area <- pivot_wider(M020_zone_section_area,names_from = zone_section_name, values_from = zone_section_area)

# merge the area-objects
M020_area_mapped <- cbind(M020_Norge_area,M020_zone_area[,-1],M020_section_area[,-1],M020_zone_section_area[,-1])

### ratio calculations
names(M020_area_mapped)
area <- data.frame(area_type=c("total", "Mellom_nordboreal", "Alpin", "Nemoral_soerboreal", "O3_O3t", "OC_O2", "C1",
                               "Mellom_nordboreal_O3_O3t","Mellom_nordboreal_OC_O2","Alpin_OC_O2","Nemoral_soerboreal_O3_O3t","Alpin_C1",
                               "Mellom_nordboreal_C1","Alpin_O3_O3t","Nemoral_soerboreal_OC_O2","Nemoral_soerboreal_C1"),
                   mapped=NA,
                   total=NA)

dim(ANO.dat)
length(unique(ANO.dat$ano_punkt_id))

# mapped area per zone/section
area[1,2] <- length(unique(ANO.dat$ano_punkt_id))*250
area[2,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Mellom_nordboreal","ano_punkt_id"]))*250
area[3,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Alpin","ano_punkt_id"]))*250
area[4,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Nemoral_soerboreal","ano_punkt_id"]))*250
area[5,2] <- length(unique(ANO.dat[ANO.dat$section_name=="O3_O3t","ano_punkt_id"]))*250
area[6,2] <- length(unique(ANO.dat[ANO.dat$section_name=="OC_O2","ano_punkt_id"]))*250
area[7,2] <- length(unique(ANO.dat[ANO.dat$section_name=="C1","ano_punkt_id"]))*250
area[8,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Mellom_nordboreal_O3_O3t","ano_punkt_id"]))*250
area[9,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Mellom_nordboreal_OC_O2","ano_punkt_id"]))*250
area[10,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Alpin_OC_O2","ano_punkt_id"]))*250
area[11,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Nemoral_soerboreal_O3_O3t","ano_punkt_id"]))*250
area[12,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Alpin_C1","ano_punkt_id"]))*250
area[13,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Mellom_nordboreal_C1","ano_punkt_id"]))*250
area[14,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Alpin_O3_O3t","ano_punkt_id"]))*250
area[15,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Nemoral_soerboreal_OC_O2","ano_punkt_id"]))*250
area[16,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Nemoral_soerboreal_C1","ano_punkt_id"]))*250

# this yields more area than the area that was actually mapped to be an ecosystem type (because parts of the area in some points was not mapped)
M020_area_mapped %>% summarise(across(Norge_area:Nemoral_soerboreal_C1 , sum, na.rm=T))

# total area per zone/section
area[1,3] <- vegzones %>% summarize(area.zones = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry()
area[2,3] <- vegzones %>% group_by(NAVN2) %>% summarize(area.zones = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(NAVN2=='Mellom_nordboreal') %>% select(area.zones)
area[3,3] <- vegzones %>% group_by(NAVN2) %>% summarize(area.zones = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(NAVN2=='Alpin') %>% select(area.zones)
area[4,3] <- vegzones %>% group_by(NAVN2) %>% summarize(area.zones = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(NAVN2=='Nemoral_soerboreal') %>% select(area.zones)
area[5,3] <- vegsections %>% group_by(NAVN2) %>% summarize(area.sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(NAVN2=='O3_O3t') %>% select(area.sections)
area[6,3] <- vegsections %>% group_by(NAVN2) %>% summarize(area.sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(NAVN2=='OC_O2') %>% select(area.sections)
area[7,3] <- vegsections %>% group_by(NAVN2) %>% summarize(area.sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(NAVN2=='C1') %>% select(area.sections)
area[8,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Mellom_nordboreal_O3_O3t') %>% select(area.zones_sections)
area[9,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Mellom_nordboreal_OC_O2') %>% select(area.zones_sections)
area[10,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Alpin_OC_O2') %>% select(area.zones_sections)
area[11,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Nemoral_soerboreal_O3_O3t') %>% select(area.zones_sections)
area[12,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Alpin_C1') %>% select(area.zones_sections)
area[13,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Mellom_nordboreal_C1') %>% select(area.zones_sections)
area[14,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Alpin_O3_O3t') %>% select(area.zones_sections)
area[15,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Nemoral_soerboreal_OC_O2') %>% select(area.zones_sections)
area[16,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Nemoral_soerboreal_C1') %>% select(area.zones_sections)

area$total <- as.numeric(area$total)
area$ratio_mapped <- area$mapped/area$total

# ratio of mapped area
M020_area_predicted <- M020_area_ratio <- M020_area_mapped

M020_area_ratio$Norge_area <- M020_area_mapped$Norge_area/area[1,2]
M020_area_ratio$Mellom_nordboreal <- M020_area_mapped$Mellom_nordboreal/area[2,2]
M020_area_ratio$Alpin <- M020_area_mapped$Alpin/area[3,2]
M020_area_ratio$Nemoral_soerboreal <- M020_area_mapped$Nemoral_soerboreal/area[4,2]
M020_area_ratio$O3_O3t <- M020_area_mapped$O3_O3t/area[5,2]
M020_area_ratio$OC_O2 <- M020_area_mapped$OC_O2/area[6,2]
M020_area_ratio$C1 <- M020_area_mapped$C1/area[7,2]
M020_area_ratio$Mellom_nordboreal_O3_O3t <- M020_area_mapped$Mellom_nordboreal_O3_O3t/area[8,2]
M020_area_ratio$Mellom_nordboreal_OC_O2 <- M020_area_mapped$Mellom_nordboreal_OC_O2/area[9,2]
M020_area_ratio$Alpin_OC_O2 <- M020_area_mapped$Alpin_OC_O2/area[10,2]
M020_area_ratio$Nemoral_soerboreal_O3_O3t <- M020_area_mapped$Nemoral_soerboreal_O3_O3t/area[11,2]
M020_area_ratio$Alpin_C1 <- M020_area_mapped$Alpin_C1/area[12,2]
M020_area_ratio$Mellom_nordboreal_C1 <- M020_area_mapped$Mellom_nordboreal_C1/area[13,2]
M020_area_ratio$Alpin_O3_O3t <- M020_area_mapped$Alpin_O3_O3t/area[14,2]
M020_area_ratio$Nemoral_soerboreal_OC_O2 <- M020_area_mapped$Nemoral_soerboreal_OC_O2/area[15,2]
M020_area_ratio$Nemoral_soerboreal_C1 <- M020_area_mapped$Nemoral_soerboreal_C1/area[16,2]

M020_area_predicted$Norge_area <- M020_area_ratio$Norge_area*area[1,3]
M020_area_predicted$Mellom_nordboreal <- M020_area_ratio$Mellom_nordboreal*area[2,3]
M020_area_predicted$Alpin <- M020_area_ratio$Alpin*area[3,3]
M020_area_predicted$Nemoral_soerboreal <- M020_area_ratio$Nemoral_soerboreal*area[4,3]
M020_area_predicted$O3_O3t <- M020_area_ratio$O3_O3t*area[5,3]
M020_area_predicted$OC_O2 <- M020_area_ratio$OC_O2*area[6,3]
M020_area_predicted$C1 <- M020_area_ratio$C1*area[7,3]
M020_area_predicted$Mellom_nordboreal_O3_O3t <- M020_area_ratio$Mellom_nordboreal_O3_O3t*area[8,3]
M020_area_predicted$Mellom_nordboreal_OC_O2 <- M020_area_ratio$Mellom_nordboreal_OC_O2*area[9,3]
M020_area_predicted$Alpin_OC_O2 <- M020_area_ratio$Alpin_OC_O2*area[10,3]
M020_area_predicted$Nemoral_soerboreal_O3_O3t <- M020_area_ratio$Nemoral_soerboreal_O3_O3t*area[11,3]
M020_area_predicted$Alpin_C1 <- M020_area_ratio$Alpin_C1*area[12,3]
M020_area_predicted$Mellom_nordboreal_C1 <- M020_area_ratio$Mellom_nordboreal_C1*area[13,3]
M020_area_predicted$Alpin_O3_O3t <- M020_area_ratio$Alpin_O3_O3t*area[14,3]
M020_area_predicted$Nemoral_soerboreal_OC_O2 <- M020_area_ratio$Nemoral_soerboreal_OC_O2*area[15,3]
M020_area_predicted$Nemoral_soerboreal_C1 <- M020_area_ratio$Nemoral_soerboreal_C1*area[16,3]




# how much area is predicted in total (of 322 653 sqkm)
M020_area_predicted %>% summarise(across(Norge_area:Nemoral_soerboreal_C1 , sum, na.rm=T)) # less area is predicted since the ANO.data includes points where only part of the area was mapped

write.csv2(M020_area_predicted,"Output/M020_area_predicted_Moen.csv")
write.csv2(M020_area_mapped,"Output/M020_area_mapped_Moen.csv")
