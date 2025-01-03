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
vegzones <- st_read("R:/GeoSpatialData/BiogeographicalRegions/Norway_VegetationZones_Moen/Original/Vector/soner.shp")
vegsections <- st_read("R:/GeoSpatialData/BiogeographicalRegions/Norway_VegetationZones_Moen/Original/Vector/seksjoner.shp",crs=st_crs(vegzones) )

plot(vegzones['NAVN'])
plot(vegsections['NAVN'])

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

plot(vegzones['KLASSE2'])
plot(vegzones['NAVN2'])



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
                          NAVN %in% c("O2-Klart oceani","O1-Svakt oceani","OC_Overgangssek")~"OC_O2",
                          NAVN %in% c("C1-Svakt kontin")~"C1"
  )
  )

plot(vegsections['KLASSE2'])
plot(vegsections['NAVN2'])


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


## remove all the points that have not been mapped at the circle level
unique(ANO.geo$kartleggingsenhet_250m2)
summary(as.factor(ANO.geo$kartleggingsenhet_250m2))
ANO.geo <- ANO.geo %>%
  mutate(across(kartleggingsenhet_250m2, ~replace(., . %in% c("", "NA", "N/A", "ikke_kartlagt"), NA)))
ANO.geo$kartleggingsenhet_250m2 <- factor(ANO.geo$kartleggingsenhet_250m2)
dim(ANO.geo)
ANO.dat <- ANO.geo %>%
  filter(!is.na(kartleggingsenhet_250m2) )
dim(ANO.dat)
summary(as.factor(ANO.dat$kartleggingsenhet_250m2))

#### translating NiN2-codes for kartleggingsenhet_250m2 (1:5000) to NiN3-codes 1:20000
ANO.dat
ANO.dat <- merge(x=ANO.dat[,c("GlobalID","ano_punkt_id","kartleggingsenhet_250m2","andel_kartleggingsenhet_250m2","lat","long")],
                 y=NiNtrans,
                 by.x="kartleggingsenhet_250m2", by.y="NIN2_kartleggingsenhet", all.x=T)
names(ANO.dat)
summary(as.factor(ANO.dat$M020_kode2))

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
  tm_fill('NAVN2', labels="", title="", legend.show = TRUE) + 
  tm_borders() +
  tm_shape(ANO.dat) +
  tm_dots()


ANO.dat <- st_join(ANO.dat,vegzones[,c("KLASSE2","NAVN2")],join= st_nearest_feature)
names(ANO.dat)[c(9,10)] <- c("zone_class","zone_name")

ANO.dat <- st_join(ANO.dat,vegsections[,c("KLASSE2","NAVN2")],join= st_nearest_feature)
names(ANO.dat)[c(11,12)] <- c("section_class","section_name")

### area calculations
# drop geometry from ANO.dat
ANO.dat <- ANO.dat %>% st_drop_geometry()

# calculate area for Norway
M020_Norge_area <- ANO.dat %>%
  group_by(M020_kode2) %>%
  summarize(Norge_area=sum(area,na.rm=T))

# calculate area for zones
M020_zone_area <- ANO.dat %>%
  group_by(M020_kode2,zone_name) %>%
  summarize(zone_area=sum(area,na.rm=T))

# calculate area for sections
M020_section_area <- ANO.dat %>%
  group_by(M020_kode2,section_name) %>%
  summarize(section_area=sum(area,na.rm=T))

## checking if there's any area for points outside zones or sections
ANO.dat[is.na(ANO.dat$zone_class),]
ANO.dat[is.na(ANO.dat$section_class),]

## make wide versions and merge them
M020_zone_area <- pivot_wider(M020_zone_area,names_from = zone_name, values_from = zone_area)
M020_section_area <- pivot_wider(M020_section_area,names_from = section_name, values_from = section_area)

# merge the area-objects
cbind(M020_Norge_area,M020_zone_area,M020_section_area)
M020_area_mapped <- cbind(M020_Norge_area,M020_zone_area[,-1],M020_section_area[,-1])

### ratio calculations
mapped_area <- data.frame(area_type=c("total", "Alpin", "Mellom_nordboreal", "Nemoral_soerboreal", "C1", "O3_O3t", "OC_O2"), 
                          area=NA)

dim(ANO.dat)
length(unique(ANO.dat$ano_punkt_id))

mapped_area[1,2] <- length(unique(ANO.dat$ano_punkt_id))*250
mapped_area[2,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Alpin","ano_punkt_id"]))*250
mapped_area[3,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Mellom_nordboreal","ano_punkt_id"]))*250
mapped_area[4,2] <- length(unique(ANO.dat[ANO.dat$zone_name=="Nemoral_soerboreal","ano_punkt_id"]))*250
mapped_area[5,2] <- length(unique(ANO.dat[ANO.dat$section_name=="C1","ano_punkt_id"]))*250
mapped_area[6,2] <- length(unique(ANO.dat[ANO.dat$section_name=="O3_O3t","ano_punkt_id"]))*250
mapped_area[7,2] <- length(unique(ANO.dat[ANO.dat$section_name=="OC_O2","ano_punkt_id"]))*250

M020_area_ratio <- M020_area_mapped

M020_area_ratio$Norge_area <- M020_area_mapped$Norge_area/mapped_area[1,2]


M020_area <- M020_area %>%
  mutate(ratio_Norge = area_Norge/mapped_area)

summary(M020_area)

M020_area[M020_area$ratio_Norge > 0.049,]
