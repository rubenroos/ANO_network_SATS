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

### Redlist
redlist <- read.csv("P:/823001_18_metodesats_analyse_23_26_roos/Tyler and Redlist/redlist2021new.txt", sep="\t", header=T)
head(redlist)
redlist <- redlist %>% 
  filter(Artsgruppe=="Karplanter") %>%
  filter(Vurderingsomraade=="Norge")

### upload vegetation zone map
veg_zones <- rast("P:/823001_18_metodesats_analyse_23_26_roos/Naturindeks_N50_vegetasjonssoner_25m.tif")



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

dim(ANO.dat)
length(unique(ANO.dat$ano_punkt_id))

M020_area <- ANO.dat %>%
  group_by(M020_kode2) %>%
  summarize(area_Norge=sum(area))

mapped_area <- length(unique(ANO.dat$ano_punkt_id))*250

M020_area <- M020_area %>%
  mutate(ratio_Norge = area_Norge/mapped_area)

summary(M020_area)

M020_area[M020_area$ratio_Norge > 0.049,]






## adding geometry
ANO.dat <- st_as_sf(ANO.dat,coords=c('lat','long'),crs=ANO.geo.crs, remove=F)

