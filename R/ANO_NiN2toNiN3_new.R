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
NiNtrans <- read_xlsx("C:/Users/joachim.topper/OneDrive - NINA/work/projects/NiN/NiN3/NiN2_5k_NiN3_20k.xlsx", sheet="Typer")
head(NiNtrans)
summary(NiNtrans)
names(NiNtrans)

NiNtrans <- NiNtrans %>% 
  filter(rødlistevurdering==1) %>%
  filter(NIN2_kartleggingsenhet!='NA' ) %>%
  filter(M020_kode2!='NA' ) %>%
  select(NIN2_kartleggingsenhet,M020_kode2)

NiNtrans <- NiNtrans %>%
  distinct(NIN2_kartleggingsenhet, .keep_all = TRUE)

#### upload maps for vegetation zones and sections
vegzones <- st_read("R:/GeoSpatialData/BiogeographicalRegions/Norway_PCA_klima/Converted/Soner2017.shp")
vegsections <- st_read("R:/GeoSpatialData/BiogeographicalRegions/Norway_PCA_klima/Converted/Seksjoner2017.shp")

# dissolve tiles
vegzones <- vegzones %>%
  group_by(Sone_navn) %>%
  summarise(geometry = st_union(geometry))

vegsections <- vegsections %>%
  group_by(Seksjon_na) %>%
  summarise(geometry = st_union(geometry))

plot(vegzones['Sone_navn'])
plot(vegsections['Seksjon_na'])

# combine certain zones and sections
vegzones <- vegzones %>%
  mutate(Sone_navn2 = case_when(Sone_navn %in% c("Boreonemoral sone (BN)", "Sørboreal sone (SB)") ~ "Boreonem_sørbor",
                                Sone_navn %in% c("Mellomboreal sone (MB)", "Nordboreal sone (NB)") ~ "Mellom_Nordbor",
                                TRUE ~ Sone_navn)
  )

vegsections <- vegsections %>%
  mutate(Seksjon_na2 = case_when(Seksjon_na %in% c("Overgangsseksjon (OC)", "Svakt oseanisk seksjon (O1)", "Klart oseanisk seksjon (O2)") ~ "Over_svakt_klart (OC-O1_O2)",
                                 TRUE ~ Seksjon_na)
  ) 

plot(vegzones['Sone_navn2'])
plot(vegsections['Seksjon_na2'])

# combining zones & sections
comb_zones_sections <- st_intersection(vegzones, vegsections)
comb_zones_sections <- st_collection_extract(comb_zones_sections, type = "POLYGON")  # For POLYGON-komponenter

comb_zones_sections <- comb_zones_sections %>%
  mutate(sone_sek = paste0(Sone_navn2,"_", Seksjon_na2))


plot(comb_zones_sections['sone_sek'])



#### data handling - ANO data ####
head(ANO.geo)

## fix NiN information on hovedtyper
ANO.geo$hovedtype_rute <- substr(ANO.geo$kartleggingsenhet_1m2,1,3) # take the 3 first characters
ANO.geo$hovedtype_rute <- gsub("-", "", ANO.geo$hovedtype_rute) # remove hyphon
unique(as.factor(ANO.geo$hovedtype_rute))

## fix NiN-variables
colnames(ANO.geo)
colnames(ANO.geo)[41:46] <- c("groeftingsintensitet",
                              "bruksintensitet",
                              "beitetrykk",
                              "slatteintensitet",
                              "tungekjoretoy",
                              "slitasje")
head(ANO.geo)

# remove variable code in the data
ANO.geo$groeftingsintensitet <- gsub("7GR-GI_", "", ANO.geo$groeftingsintensitet) 
unique(ANO.geo$groeftingsintensitet)
ANO.geo$groeftingsintensitet <- gsub("X", "NA", ANO.geo$groeftingsintensitet)
unique(ANO.geo$groeftingsintensitet)
ANO.geo$groeftingsintensitet <- as.numeric(ANO.geo$groeftingsintensitet)
unique(ANO.geo$groeftingsintensitet)

ANO.geo$bruksintensitet <- gsub("7JB-BA_", "", ANO.geo$bruksintensitet) 
unique(ANO.geo$bruksintensitet)
ANO.geo$bruksintensitet <- gsub("X", "NA", ANO.geo$bruksintensitet)
unique(ANO.geo$bruksintensitet)
ANO.geo$bruksintensitet <- as.numeric(ANO.geo$bruksintensitet)
unique(ANO.geo$bruksintensitet)

ANO.geo$beitetrykk <- gsub("7JB-BT_", "", ANO.geo$beitetrykk) 
unique(ANO.geo$beitetrykk)
ANO.geo$beitetrykk <- gsub("X", "NA", ANO.geo$beitetrykk)
unique(ANO.geo$beitetrykk)
ANO.geo$beitetrykk <- as.numeric(ANO.geo$beitetrykk)
unique(ANO.geo$beitetrykk)

ANO.geo$slatteintensitet <- gsub("7JB-SI_", "", ANO.geo$slatteintensitet) 
unique(ANO.geo$slatteintensitet)
ANO.geo$slatteintensitet <- gsub("X", "NA", ANO.geo$slatteintensitet)
unique(ANO.geo$slatteintensitet)
ANO.geo$slatteintensitet <- as.numeric(ANO.geo$slatteintensitet)
unique(ANO.geo$slatteintensitet)

ANO.geo$tungekjoretoy <- gsub("7TK_", "", ANO.geo$tungekjoretoy) 
unique(ANO.geo$tungekjoretoy)
ANO.geo$tungekjoretoy <- gsub("X", "NA", ANO.geo$tungekjoretoy)
unique(ANO.geo$tungekjoretoy)
ANO.geo$tungekjoretoy <- as.numeric(ANO.geo$tungekjoretoy)
unique(ANO.geo$tungekjoretoy)

ANO.geo$slitasje <- gsub("7SE_", "", ANO.geo$slitasje) 
unique(ANO.geo$slitasje)
ANO.geo$slitasje <- gsub("X", "NA", ANO.geo$slitasje)
unique(ANO.geo$slitasje)
ANO.geo$slitasje <- as.numeric(ANO.geo$slitasje)
unique(ANO.geo$slitasje)

## check that every point is present only once
length(levels(as.factor(ANO.geo$ano_flate_id)))
length(levels(as.factor(ANO.geo$ano_punkt_id)))
summary(as.factor(ANO.geo$ano_punkt_id))
# there's a triple and many double presences, 
# probably some wrong registrations of point numbers, but also double registrations
# there's even entire sites (flater) with all points double
#write.csv(summary(as.factor(ANO.geo$ano_punkt_id), maxsum=330), "Output/ANO.punktfrekvens.csv")


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

ANO.dat[,c(1,4,7)]

ANO.dat$area <- ANO.dat$andel_kartleggingsenhet_250m2/100*250

ANO.dat[,c(1,4,7,8)]

## adding geometry
ANO.dat <- st_as_sf(ANO.dat,coords=c('lat','long'),crs=ANO.geo.crs, remove=F)






#### area of types in vegzones and vegsections ####

### adding vegzones & vesections to ANO.dat
# change the CRS of ANO.dat to the CRS of the vegzones and -sections maps
ANO.dat <- st_transform(ANO.dat, crs = st_crs(vegzones))


tm_shape(vegzones) +
  tm_fill('Sone_navn2', labels="", title="", legend.show = TRUE) + 
  tm_borders() +
  tm_shape(ANO.dat) +
  tm_dots()

tm_shape(vegsections) +
  tm_fill('Seksjon_na2', labels="", title="", legend.show = TRUE) + 
  tm_borders() +
  tm_shape(ANO.dat) +
  tm_dots()


ANO.dat <- st_join(ANO.dat,vegzones[,c("Sone_navn2")],join= st_nearest_feature)
names(ANO.dat)[c(10)] <- c("zone_name")

ANO.dat <- st_join(ANO.dat,vegsections[,c("Seksjon_na2")],join= st_nearest_feature)
names(ANO.dat)[c(11)] <- c("section_name")

ANO.dat <- st_join(ANO.dat,comb_zones_sections[,c("sone_sek")],join= st_nearest_feature)
names(ANO.dat)[c(12)] <- c("zone_section_name")

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
cbind(M020_Norge_area,M020_zone_area,M020_section_area)
M020_area_mapped <- cbind(M020_Norge_area,M020_zone_area[,-1],M020_section_area[,-1],M020_zone_section_area[,-1])

### ratio calculations
names(M020_area_mapped)
area <- data.frame(area_type=c("total", "Alpin", "Mellom_nordboreal", "Boreonem_soerboreal", "OC_O2", "O3", "C1",
                               "Alpin_OC_O2", "Mellom_nordboreal_O3", "Mellom_nordboreal_OC_O2", "Alpin_C1", 
                               "Boreonem_soerboreal_OC_O2)", "Boreonem_soerboreal_O3", "Mellom_nordboreal_C1"),
                   mapped=NA,
                   total=NA)

dim(ANO.dat)
length(unique(ANO.dat$ano_punkt_id))

# mapped area per zone/section
area[1,2] <- length(unique(ANO.dat$ano_punkt_id))*250
area[2,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Lavalpin sone (LA)","ano_punkt_id"]))*250
area[3,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Mellom_Nordbor","ano_punkt_id"]))*250
area[4,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Boreonem_sørbor","ano_punkt_id"]))*250
area[5,2] <- length(unique(ANO.dat[ANO.dat$section_name=="Over_svakt_klart (OC-O1_O2)","ano_punkt_id"]))*250
area[6,2] <- length(unique(ANO.dat[ANO.dat$section_name=="Sterkt oseanisk seksjon (O3)","ano_punkt_id"]))*250
area[7,2] <- length(unique(ANO.dat[ANO.dat$section_name=="Svakt kontinental seksjon (C1)","ano_punkt_id"]))*250

area[8,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Lavalpin sone (LA)_Over_svakt_klart (OC-O1_O2)","ano_punkt_id"]))*250
area[9,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Mellom_Nordbor_Sterkt oseanisk seksjon (O3)","ano_punkt_id"]))*250
area[10,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Mellom_Nordbor_Over_svakt_klart (OC-O1_O2)","ano_punkt_id"]))*250
area[11,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Lavalpin sone (LA)_Svakt kontinental seksjon (C1)","ano_punkt_id"]))*250
area[12,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Boreonem_sørbor_Over_svakt_klart (OC-O1_O2)","ano_punkt_id"]))*250
area[13,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Boreonem_sørbor_Sterkt oseanisk seksjon (O3)","ano_punkt_id"]))*250
area[14,2] <- length(unique(ANO.dat[ANO.dat$zone_section_name=="Mellom_Nordbor_Svakt kontinental seksjon (C1)","ano_punkt_id"]))*250
# this yields more area than the area that was actually mapped to be an ecosystem type (because parts of the area in some points was not mapped)
M020_area_mapped %>% summarise(across(Norge_area:'Mellom_Nordbor_Svakt kontinental seksjon (C1)' , sum, na.rm=T))

# total area per zone/section
vegzones <- vegzones %>% mutate(AREA = st_area(geometry))
vegsections <- vegsections %>% mutate(AREA = st_area(geometry))
comb_zones_sections <- comb_zones_sections %>% mutate(AREA = st_area(geometry))

area[1,3] <- vegzones %>% summarize(area.zones = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry()
area[2,3] <- vegzones %>% group_by(Sone_navn2) %>% summarize(area.zones = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(Sone_navn2=='Lavalpin sone (LA)') %>% select(area.zones)
area[3,3] <- vegzones %>% group_by(Sone_navn2) %>% summarize(area.zones = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(Sone_navn2=='Mellom_Nordbor') %>% select(area.zones)
area[4,3] <- vegzones %>% group_by(Sone_navn2) %>% summarize(area.zones = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(Sone_navn2=='Boreonem_sørbor') %>% select(area.zones)
area[5,3] <- vegsections %>% group_by(Seksjon_na2) %>% summarize(area.sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(Seksjon_na2=='Over_svakt_klart (OC-O1_O2)') %>% select(area.sections)
area[6,3] <- vegsections %>% group_by(Seksjon_na2) %>% summarize(area.sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(Seksjon_na2=='Sterkt oseanisk seksjon (O3)') %>% select(area.sections)
area[7,3] <- vegsections %>% group_by(Seksjon_na2) %>% summarize(area.sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(Seksjon_na2=='Svakt kontinental seksjon (C1)') %>% select(area.sections)

area[8,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Lavalpin sone (LA)_Over_svakt_klart (OC-O1_O2)') %>% select(area.zones_sections)
area[9,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Mellom_Nordbor_Sterkt oseanisk seksjon (O3)') %>% select(area.zones_sections)
area[10,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Mellom_Nordbor_Over_svakt_klart (OC-O1_O2)') %>% select(area.zones_sections)
area[11,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Lavalpin sone (LA)_Svakt kontinental seksjon (C1)') %>% select(area.zones_sections)
area[12,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Boreonem_sørbor_Over_svakt_klart (OC-O1_O2)') %>% select(area.zones_sections)
area[13,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Boreonem_sørbor_Sterkt oseanisk seksjon (O3)') %>% select(area.zones_sections)
area[14,3] <- comb_zones_sections %>% group_by(sone_sek) %>% summarize(area.zones_sections = sum(AREA, na.rm = TRUE)) %>% st_drop_geometry() %>% filter(sone_sek=='Mellom_Nordbor_Svakt kontinental seksjon (C1)') %>% select(area.zones_sections)


area$total <- as.numeric(area$total)
area$ratio_mapped <- area$mapped/area$total

# ratio of mapped area
M020_area_predicted <- M020_area_ratio <- M020_area_mapped
names(M020_area_mapped)

M020_area_ratio$Norge_area <- M020_area_mapped$Norge_area/area[1,2]
M020_area_ratio$'Lavalpin sone (LA)' <- M020_area_mapped$'Lavalpin sone (LA)'/area[2,2]
M020_area_ratio$Mellom_Nordbor <- M020_area_mapped$Mellom_Nordbor/area[3,2]
M020_area_ratio$Boreonem_sørbor <- M020_area_mapped$Boreonem_sørbor/area[4,2]
M020_area_ratio$'Over_svakt_klart (OC-O1_O2)' <- M020_area_mapped$'Over_svakt_klart (OC-O1_O2)'/area[5,2]
M020_area_ratio$'Sterkt oseanisk seksjon (O3)' <- M020_area_mapped$'Sterkt oseanisk seksjon (O3)'/area[6,2]
M020_area_ratio$'Svakt kontinental seksjon (C1)' <- M020_area_mapped$'Svakt kontinental seksjon (C1)'/area[7,2]
M020_area_ratio$'Lavalpin sone (LA)_Over_svakt_klart (OC-O1_O2)' <- M020_area_mapped$'Lavalpin sone (LA)_Over_svakt_klart (OC-O1_O2)'/area[8,2]
M020_area_ratio$'Mellom_Nordbor_Sterkt oseanisk seksjon (O3)' <- M020_area_mapped$'Mellom_Nordbor_Sterkt oseanisk seksjon (O3)'/area[9,2]
M020_area_ratio$'Mellom_Nordbor_Over_svakt_klart (OC-O1_O2)' <- M020_area_mapped$'Mellom_Nordbor_Over_svakt_klart (OC-O1_O2)'/area[10,2]
M020_area_ratio$'Lavalpin sone (LA)_Svakt kontinental seksjon (C1)' <- M020_area_mapped$'Lavalpin sone (LA)_Svakt kontinental seksjon (C1)'/area[11,2]
M020_area_ratio$'Boreonem_sørbor_Over_svakt_klart (OC-O1_O2)' <- M020_area_mapped$'Boreonem_sørbor_Over_svakt_klart (OC-O1_O2)'/area[12,2]
M020_area_ratio$'Boreonem_sørbor_Sterkt oseanisk seksjon (O3)' <- M020_area_mapped$'Boreonem_sørbor_Sterkt oseanisk seksjon (O3)'/area[13,2]
M020_area_ratio$'Mellom_Nordbor_Svakt kontinental seksjon (C1)' <- M020_area_mapped$'Mellom_Nordbor_Svakt kontinental seksjon (C1)'/area[14,2]

M020_area_predicted$Norge_area <- M020_area_ratio$Norge_area*area[1,3]
M020_area_predicted$'Lavalpin sone (LA)' <- M020_area_ratio$'Lavalpin sone (LA)'*area[2,3]
M020_area_predicted$Mellom_Nordbor <- M020_area_ratio$Mellom_Nordbor*area[3,3]
M020_area_predicted$Boreonem_sørbor <- M020_area_ratio$Boreonem_sørbor*area[4,3]
M020_area_predicted$'Over_svakt_klart (OC-O1_O2)' <- M020_area_ratio$'Over_svakt_klart (OC-O1_O2)'*area[5,3]
M020_area_predicted$'Sterkt oseanisk seksjon (O3)' <- M020_area_ratio$'Sterkt oseanisk seksjon (O3)'*area[6,3]
M020_area_predicted$'Svakt kontinental seksjon (C1)' <- M020_area_ratio$'Svakt kontinental seksjon (C1)'*area[7,3]
M020_area_predicted$'Lavalpin sone (LA)_Over_svakt_klart (OC-O1_O2)' <- M020_area_ratio$'Lavalpin sone (LA)_Over_svakt_klart (OC-O1_O2)'*area[8,3]
M020_area_predicted$'Mellom_Nordbor_Sterkt oseanisk seksjon (O3)' <- M020_area_ratio$'Mellom_Nordbor_Sterkt oseanisk seksjon (O3)'*area[9,3]
M020_area_predicted$'Mellom_Nordbor_Over_svakt_klart (OC-O1_O2)' <- M020_area_ratio$'Mellom_Nordbor_Over_svakt_klart (OC-O1_O2)'*area[10,3]
M020_area_predicted$'Lavalpin sone (LA)_Svakt kontinental seksjon (C1)' <- M020_area_ratio$'Lavalpin sone (LA)_Svakt kontinental seksjon (C1)'*area[11,3]
M020_area_predicted$'Boreonem_sørbor_Over_svakt_klart (OC-O1_O2)' <- M020_area_ratio$'Boreonem_sørbor_Over_svakt_klart (OC-O1_O2)'*area[12,3]
M020_area_predicted$'Boreonem_sørbor_Sterkt oseanisk seksjon (O3)' <- M020_area_ratio$'Boreonem_sørbor_Sterkt oseanisk seksjon (O3)'*area[13,3]
M020_area_predicted$'Mellom_Nordbor_Svakt kontinental seksjon (C1)' <- M020_area_ratio$'Mellom_Nordbor_Svakt kontinental seksjon (C1)'*area[14,3]



# how much area is predicted in total (of 322 653 sqkm)
M020_area_predicted %>% summarise(across(Norge_area:'Mellom_Nordbor_Svakt kontinental seksjon (C1)' , sum, na.rm=T)) # less area is predicted since the ANO.data includes points where only part of the area was mapped

M020_area <- M020_area %>%
  mutate(ratio_Norge = area_Norge/mapped_area)

summary(M020_area)

M020_area[M020_area$ratio_Norge > 0.049,]


write.csv2(M020_area_predicted,"Output/M020_area_predicted_new.csv")
write.csv2(M020_area_mapped,"Output/M020_area_mapped_new.csv")
