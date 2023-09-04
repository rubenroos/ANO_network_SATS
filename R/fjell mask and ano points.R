

nor <- st_read("input/outlineOfNorway_EPSG25833.shp")%>%
  st_as_sf() %>%
  st_transform(crs = st_crs(ANO.geo))

reg <- st_read("input/regions.shp")%>%
  st_as_sf() %>%
  st_transform(crs = st_crs(ANO.geo))

# change region names to something R-friendly
reg$region
reg$region <- c("Northern Norway","Central Norway","Eastern Norway","Western Norway","Southern Norway")

regnor <- st_intersection(reg,nor)


file <- "P:/41201042_okologisk_tilstand_fastlandsnorge_2020_dataanaly/M/2021 Fjell/Mountain ecosystem Norge 50m.tif"
fjell <- read_stars(file)
fjell


fjell2 <- readRDS("input/fjell_1km.rds")

fjell <- st_as_stars(fjell)
fjell[fjell[]==0] <- NA



tm_shape(fjell2) +
  tm_raster()

zion_extract = terra::extract(srtm, vect(zion_points))

terra::extract(fjell, vect(ANO.geo))


