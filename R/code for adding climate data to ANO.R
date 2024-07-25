### upload climate data
# worldclim data imported from ECoMAP's p-folder
# the data are 10-yr climatologies from 1967-2017
# here we use the most recent one from 2017
rastlist <- list.files(path = "P:/22202210_ecomap/explanatory variables/Climate/HighRes", pattern='.Tif$', all.files= T, full.names= T)
climate <- list()
climate[['bio1']]  <- terra::rast(rastlist[1])[['C17']]
climate[['bio10']]  <- terra::rast(rastlist[2])[['C17']]
climate[['bio11']]  <- terra::rast(rastlist[3])[['C17']]
climate[['bio12']]  <- terra::rast(rastlist[4])[['C17']]
climate[['bio16']]  <- terra::rast(rastlist[5])[['C17']]
climate[['bio17']]  <- terra::rast(rastlist[6])[['C17']]
climate[['bio5']]  <- terra::rast(rastlist[7])[['C17']]
climate[['bio6']]  <- terra::rast(rastlist[8])[['C17']]

# worldclim variable codes
#BIO1 = Annual Mean Temperature
#BIO10 = Mean Temperature of Warmest Quarter
#BIO11 = Mean Temperature of Coldest Quarter
#BIO12 = Annual Precipitation
#BIO16 = Precipitation of Wettest Quarter
#BIO17 = Precipitation of Driest Quarter
#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month

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
colnames(ANO.geo)[68:75] <- c('bio1','bio10','bio11','bio12','bio16','bio17','bio5','bio6')


