library(sf)
library(terra)

buildPlan <- st_read("P:/823001_18_metodesats_analyse_23_26_roos/Planlagt-Utbygging2023_0000_Norge_25833_GPKG.gpkg")

veg_zones <- terra::rast("P:/823001_18_metodesats_analyse_23_26_roos/Naturindeks_N50_vegetasjonssoner_25m.tif")


buildPlan <- buildPlan %>% st_transform(crs = st_crs(veg_zones))


val <- terra::extract(veg_zones, vect(buildPlan), fun=table, exact=TRUE, ID=TRUE, bind=TRUE)





