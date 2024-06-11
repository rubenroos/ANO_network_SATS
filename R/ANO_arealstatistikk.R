#### Arealstatistikk fra ANO

# fix NiN information on hovedtyper
ANO.geo$hovedtype_rute <- substr(ANO.geo$kartleggingsenhet_1m2,1,3) # take the 3 first characters
ANO.geo$hovedtype_rute <- gsub("-", "", ANO.geo$hovedtype_rute) # remove hyphon
unique(as.factor(ANO.geo$hovedtype_rute))

# upload region-masken
reg <- st_read("P:/41201785_okologisk_tilstand_2022_2023/data/regioner/regNorway_wgs84 - MERGED.shp")%>%
  st_as_sf() %>%
  st_transform(crs = st_crs(ANO.geo))

colnames(ANO.geo)
ANO.geo = st_join(ANO.geo, reg, left = TRUE)
colnames(ANO.geo)

ANO_punktstat_regioner <- ANO.geo %>%
  group_by(hovedtype_rute, region) %>%
  count() %>%
  pivot_wider(names_from = "region", values_from = "n") %>%
  st_drop_geometry()

write.table(ANO_punktstat_regioner, file='C:/Users/joachim.topper/OneDrive - NINA/work/R projects/github/ANO_network_SATS/Output/ANO_punktstat_regioner.txt',quote=FALSE,sep=";",col.names=TRUE,row.names=FALSE,dec=".")

ANO_punktstat <- ANO.geo %>%
  group_by(hovedtype_rute) %>%
  count() %>%
#  pivot_wider(names_from = "region", values_from = "n") %>%
  st_drop_geometry()

write.table(ANO_punktstat, file='C:/Users/joachim.topper/OneDrive - NINA/work/R projects/github/ANO_network_SATS/Output/ANO_punktstat.txt',quote=FALSE,sep=";",col.names=TRUE,row.names=FALSE,dec=".")

ANO_flatestat <- ANO.geo %>%
  group_by(hovedtype_rute) %>%
  count(ano_flate_id) %>%
  #  pivot_wider(names_from = "region", values_from = "n") %>%
  st_drop_geometry()

ANO_flatestat <- ANO_flatestat %>%
  group_by(hovedtype_rute) %>%
  count()

write.table(ANO_flatestat, file='C:/Users/joachim.topper/OneDrive - NINA/work/R projects/github/ANO_network_SATS/Output/ANO_flatestat.txt',quote=FALSE,sep=";",col.names=TRUE,row.names=FALSE,dec=".")
