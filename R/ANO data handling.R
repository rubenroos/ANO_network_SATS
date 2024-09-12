library(sf)
library(tmap)
library(tidyverse)
library(terra)

#### load data ####
### download from kartkatalogen to P-drive
#url <- "https://nedlasting.miljodirektoratet.no/naturovervaking/naturovervaking_eksport.gdb.zip"
#download(url, dest="P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb.zip", mode="w") 
#unzip ("P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb.zip", 
#       exdir = "P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb")

### upload data from P-drive
## ANO
#st_layers(dsn = "P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb")
ANO.sp <- st_read("P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb", layer="ANO_Art")
ANO.geo <- st_read("P:/823001_18_metodesats_analyse_23_26_roos/ANO data/naturovervaking_eksport.gdb", layer="ANO_SurveyPoint")

### upload climate data
# worldclim data at 1 km resolution from rawdata provided by SeNorge (claculations and curation courtesy of ECoMAP)
# the data are 10-yr climatologies from 1967-2017
# here we use the most recent one from 2017
rastlist <- list.files(path = "P:/823001_18_metodesats_analyse_23_26_roos/climate data", pattern='.Tif$', all.files= T, full.names= T)
library(gtools)
rastlist = mixedsort(sort(rastlist))
climate <- list()
climate[['bio1']]  <- terra::rast(rastlist[1])[['period6']]
climate[['bio10']]  <- terra::rast(rastlist[2])[['period6']]
climate[['bio11']]  <- terra::rast(rastlist[3])[['period6']]
climate[['bio12']]  <- terra::rast(rastlist[4])[['period6']]
climate[['bio16']]  <- terra::rast(rastlist[5])[['period6']]
climate[['bio17']]  <- terra::rast(rastlist[6])[['period6']]


# worldclim variable codes
#BIO1 = Annual Mean Temperature
#BIO10 = Mean Temperature of Warmest Quarter
#BIO11 = Mean Temperature of Coldest Quarter
#BIO12 = Annual Precipitation
#BIO16 = Precipitation of Wettest Quarter
#BIO17 = Precipitation of Driest Quarter


## adding climate
# extract climate info for ANO points and merge with ANO.geo
for (i in 1:length(climate) ) {
  # change ANO-crs (vector) to climate crs (raster)
  ANO.geo.clim <- ANO.geo %>% st_transform(crs = st_crs(climate[[i]]))
  # extraxt climate for ANO-points
  ANO_biox <- terra::extract(climate[[i]], vect(ANO.geo.clim))
  # add climate to ANO.geo data
  ANO.geo <- cbind(ANO.geo, ANO_biox[,2])
}
colnames(ANO.geo)
colnames(ANO.geo)[68:73] <- names(climate)

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


### Redlist
redlist <- read.csv("P:/823001_18_metodesats_analyse_23_26_roos/Tyler and Redlist/redlist2021new.txt", sep="\t", header=T)
head(redlist)
redlist <- redlist %>% 
  filter(Artsgruppe=="Karplanter") %>%
  filter(Vurderingsomraade=="Norge")

### upload vegetation zone map
veg_zones <- rast("P:/823001_18_metodesats_analyse_23_26_roos/Naturindeks_N50_vegetasjonssoner_25m.tif")

### Tyler indicator data
ind.Tyler <- read.table("P:/823001_18_metodesats_analyse_23_26_roos/Tyler and Redlist/ind_Tyler.txt", sep="\t", header=T)
head(ind.Tyler)

names(ind.Tyler)[1] <- 'species'
ind.Tyler$species <- as.factor(ind.Tyler$species)
summary(ind.Tyler$species)
ind.Tyler <- ind.Tyler[!is.na(ind.Tyler$species),]
ind.Tyler[,'species.orig'] <- ind.Tyler[,'species']
ind.Tyler[,'species'] <- word(ind.Tyler[,'species'], 1,2)

#ind.Tyler2 <- ind.Tyler
#ind.Tyler <- ind.Tyler2
ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]
ind.Tyler.dup <- ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]
ind.Tyler[ind.Tyler$species %in% ind.Tyler.dup,c("Heat_requirement","Cold_requirement","species.orig","species")]
ind.Tyler <- ind.Tyler %>% filter( !(species.orig %in% list("Ammophila arenaria x Calamagrostis epigejos",
                                                            "Anemone nemorosa x ranunculoides",
                                                            "Armeria maritima ssp. elongata",
                                                            "Asplenium trichomanes ssp. quadrivalens",
                                                            "Calystegia sepium ssp. spectabilis",
                                                            "Campanula glomerata 'Superba'",
                                                            "Dactylorhiza maculata ssp. fuchsii",
                                                            "Erigeron acris ssp. droebachensis",
                                                            "Erigeron acris ssp. politus",
                                                            "Erysimum cheiranthoides L. ssp. alatum",
                                                            "Euphrasia nemorosa x stricta var. brevipila",
                                                            "Galium mollugo x verum",
                                                            "Geum rivale x urbanum",
                                                            "Hylotelephium telephium (ssp. maximum)",
                                                            "Juncus alpinoarticulatus ssp. rariflorus",
                                                            "Lamiastrum galeobdolon ssp. argentatum",
                                                            "Lathyrus latifolius ssp. heterophyllus",
                                                            "Medicago sativa ssp. falcata",
                                                            "Medicago sativa ssp. x varia",
                                                            "Monotropa hypopitys ssp. hypophegea",
                                                            "Ononis spinosa ssp. hircina",
                                                            "Ononis spinosa ssp. procurrens",
                                                            "Pilosella aurantiaca ssp. decolorans",
                                                            "Pilosella aurantiaca ssp. dimorpha",
                                                            "Pilosella cymosa ssp. gotlandica",
                                                            "Pilosella cymosa ssp. praealta",
                                                            "Pilosella officinarum ssp. peleteranum",
                                                            "Poa x jemtlandica (Almq.) K. Richt.",
                                                            "Poa x herjedalica Harry Sm.",
                                                            "Ranunculus peltatus ssp. baudotii",
                                                            "Sagittaria natans x sagittifolia",
                                                            "Salix repens ssp. rosmarinifolia",
                                                            "Stellaria nemorum L. ssp. montana",
                                                            "Trichophorum cespitosum ssp. germanicum")
) )
ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]
ind.Tyler.dup <- ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]
ind.Tyler[ind.Tyler$species %in% ind.Tyler.dup,c("Heat_requirement","Cold_requirement","species.orig","species")]

# only Hieracium and hybrids left -> get rid of these
ind.Tyler <- ind.Tyler[!duplicated(ind.Tyler[,'species']),]
ind.Tyler[duplicated(ind.Tyler[,'species']),"species"]

ind.Tyler$species <- as.factor(ind.Tyler$species)
summary(ind.Tyler$species)
# no duplicates left


#### data handling - ANO data ####
head(ANO.sp)
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

# fix species variable
ANO.sp$Species <- ANO.sp$art_navn
unique(as.factor(ANO.sp$Species))
#ANO.sp$Species <- sub(".*?_", "", ANO.sp$Species) # lose the Norwegian name in the front
ANO.sp[,'Species'] <- word(ANO.sp[,'Species'], 1,2) # lose subspecies
ANO.sp$Species <- str_to_title(ANO.sp$Species) # make first letter capital
#ANO.sp$Species <- gsub("_", " ", ANO.sp$Species) # replace underscore with space
ANO.sp$Species <- gsub("( .*)","\\L\\1",ANO.sp$Species,perl=TRUE) # make capital letters after hyphon to lowercase
ANO.sp$Species <- gsub("( .*)","\\L\\1",ANO.sp$Species,perl=TRUE) # make capital letters after space to lowercase
unique(as.factor(ANO.sp$Species))
# remove hybrids
ANO.sp <- ANO.sp %>% 
  filter(!str_detect(Species,'×')) #symbol "×" not recognized by my version of R, so these species ar enot removed

# remove NA's from Species column
ANO.sp <- ANO.sp %>% filter(!is.na(Species))

# fix typos in species names
ANO.sp <- ANO.sp %>% 
  mutate(Species=str_replace(Species,"Linnaea borealis", "Linnea borealis"))
ANO.sp <- ANO.sp %>% 
  mutate(Species=str_replace(Species,"Carex simpliciuscula", "Kobresia simpliciuscula"))
ANO.sp <- ANO.sp %>% 
  mutate(Species=str_replace(Species,"Chamerion angustifolium", "Chamaenerion simpliciuscula"))
ANO.sp <- ANO.sp %>% 
  mutate(Species=str_replace(Species,"Helictotrichon", "Avenula"))
ANO.sp <- ANO.sp %>% 
  mutate(Species=str_replace(Species,"Taraxacum croceum", "Taraxacum crocea"))
ANO.sp <- ANO.sp %>% 
  mutate(Species=str_replace(Species,"Arctous alpinus", "Arctous alpina"))

# implement Odd's suggestions for taxonomy
# things to remove
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Agrostis vinealis'))
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Carex sp.'))
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Cotoneaster sp.'))
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Epilobium sp.'))
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Euphrasia sp.'))
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Festuca sp.'))
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Juncus sp.'))
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Poa sp.'))
ANO.sp <- ANO.sp %>% filter(!str_detect(Species, 'Salix sp.'))

# things to aggregate
ANO.sp$Species <- gsub(".*Hieracium.*", "Hieracium sp.", ANO.sp$Species)

ANO.sp <- ANO.sp %>% 
  mutate(Species=str_replace(Species,"Alchemilla alpina", "Blchemilla alpina"))
ANO.sp$Species <- gsub(".*Alchemilla.*", "Alchemilla sp.", ANO.sp$Species)
ANO.sp <- ANO.sp %>% 
  mutate(Species=str_replace(Species,"Blchemilla alpina", "Alchemilla alpina"))

head(ANO.sp)

## add information on Cold requirement and redlist status for each species
ANO.sp <- merge(x=ANO.sp[,c("ParentGlobalID","Species","art_dekning")],
                y=ind.Tyler[,c("species","Cold_requirement")],
                by.x="Species", by.y="species", all.x=T)

ANO.sp <- merge(x=ANO.sp[,c("ParentGlobalID","Species","art_dekning","Cold_requirement")],
                y=redlist[,c("Vitenskapelig.navn","Kategori.2021")],
                by.x="Species", by.y="Vitenskapelig.navn", all.x=T)

## adding information on ecosystem and condition variables
# "GlobalID" in ANO.geo does for the time being not match anything in "ParentGlobalID" in ANO.sp (seems to be a bug in the original data)
# but the url in the column "vedlegg_url" in ANO.geo contains the matching ID -> create a GlobalID2 from that to do the matching
#head(ANO.geo$vedlegg_url)
#ANO.geo$GlobalID2 <- gsub("https://vedleggapi.miljodirektoratet.no/api/overvakingvedlegg/list/", "", ANO.geo$vedlegg_url)
#unique(ANO.geo$GlobalID2)
#unique(ANO.sp$ParentGlobalID)
#ANO.sp$ParentGlobalID2 <- gsub("[{}]", "", as.factor(ANO.sp$ParentGlobalID))


ANO.dat <- merge(x=ANO.sp[,c("ParentGlobalID","Species","art_dekning","Cold_requirement","Kategori.2021")],
                 y=ANO.geo[,c("GlobalID","ano_flate_id","ano_punkt_id","lat","long","ssb_id","aar",
                              "hovedtype_rute","kartleggingsenhet_1m2",
                              "groeftingsintensitet","bruksintensitet","beitetrykk","slatteintensitet",
                              "tungekjoretoy","slitasje",
                              "vedplanter_total_dekning","busker_dekning","tresjikt_dekning","roesslyng_dekning",
                              "bio1","bio10","bio11","bio12","bio16","bio17"
                 )],#removed "SHAPE" which didn't exist in the dataset
                 by.x="ParentGlobalID", by.y="GlobalID", all.x=T)
# y: "hovedoekosystem_punkt",
names(ANO.dat)

# remove communities which did not match an ANO point (should not happen, but does)
dim(ANO.dat[is.na(ANO.dat$ano_punkt_id),])
dim(ANO.dat[!is.na(ANO.dat$ano_punkt_id),])
ANO.dat <- ANO.dat[!is.na(ANO.dat$ano_punkt_id),]

test <- filter(ANO.dat, ano_punkt_id == "ANO0436_66")

## adding geometry
ANO.dat <- st_as_sf(ANO.dat,coords=c('lat','long'),crs=ANO.geo.crs, remove=F)

#### ANO data for mountain ecosystems ####
terra::plot(veg_zones)
## adding vegetation zone info to ANO data
# change ANO-crs (vector) to veg-zone crs (raster)
ANO.dat <- ANO.dat %>% st_transform(crs = st_crs(veg_zones))
# extract veg-zone info for ANO points and merge with ANO.dat
ANO_veg_zones <- terra::extract(veg_zones, vect(ANO.dat))
ANO.dat <- cbind(ANO.dat, ANO_veg_zones[,2])
rm(ANO_veg_zones)
colnames(ANO.dat)[30] <- "veg_zone"

# filter out all veg-zones that are not alpine
ANO.fjell <- ANO.dat %>% filter(veg_zone>=2)

# making it into a wider format, remove duplicates first
### note that Cold_requirement and Kategori.2021 are species specific! So, spreading out the species in a wide format makes these two columns useless ###
#ANO.fjell_w <- ANO.fjell %>% 
#  distinct(ano_punkt_id,Species, .keep_all = TRUE) %>% #added this line to remove duplicates
#  pivot_wider(names_from="Species",values_from="art_dekning")
#names(ANO.fjell_w)

## check that every point is present only once
#length(levels(as.factor(ANO.fjell$ano_flate_id)))
#length(levels(as.factor(ANO.fjell$ano_punkt_id)))
#summary(as.factor(ANO.fjell$ano_punkt_id))

## write data
#write.csv(ANO.dat, "Data/ANO.dat.RR.csv")
write.csv(ANO.fjell, "Data/ANO.data.fjell.csv")
