
# What ----

# Functions for accessing and downloading data from NVEs GridTimeSeries (GTS) data at nve.api.no. These data is at a resolution of 1x1 km and is in the UTM 33 coordinate system (EPSG: 25833).

# NOTE: These functions have been migrated into an R package, rgts.


#



# Libraries required for running examples ----

library(sf)

#



# Convert sf object to json rings with spatialReference ----

# This function is adapted from  MatthewJWhittle/getarc on github.
# https://github.com/MatthewJWhittle/getarc/tree/master
# This function accepts an sf object and converts it to json (not geojson). This format is required to collect data from senorge for a given polygon.

sf_to_json <- function(x) {
  # x: an sf of sfc object
  # Returns a character string of json
  
  # Libraries
  require(dplyr)
  require(sf)
  require(glue)
  require(stringr)
  require(geojsonsf)
  
  # make table with json types
  sf_json_type <- data.frame(sf = c("POINT", "MULTIPOINT", "LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON"), json = c("point", "points", "paths", "paths", "rings", "rings"))
  
  # Drop the z & m dimensions as I don't know how to tell if they are present
  x <- sf::st_zm(x, drop = TRUE)
  # Check the geom type  to conver to its json spec (rings, points , paths)
  x_geom_type <- sf::st_geometry_type(x)
  # Don't query if the geom type isn't supported
  stopifnot(x_geom_type %in% sf_json_type$sf)
  json_type <- dplyr::filter(sf_json_type, .data$sf == x_geom_type)$json
  
  # Convert the boundary to an sfc objet
  x_sfc <- sf::st_geometry(x)
  stopifnot(length(x_sfc) == 1)
  
  # Extract the EPSG code
  crs <- sf::st_crs(x_sfc)$epsg
  # If the geometry type is a point, the function needs to return the json in a different way
  if(json_type == "point"){
    xy <- sf::st_coordinates(x)
    return(
      glue::glue("{'x':(xy[,1]), 'y':(xy[,2]), 'spatialReference' : {'wkid' : (crs)}}",
                 .open = "(",
                 .close = ")"
      )
    )
  }
  
  
  # First convert the boundary to geojson as this is closer to the required format
  x_geojson <- geojsonsf::sfc_geojson(x_sfc)
  
  # Strip out everything outside the rings
  rings <- stringr::str_remove_all(x_geojson, "\\{.+:|\\}")
  
  # sfc_geojson adds one too many pairs of enclosing brackets
  # Its neccessary to remove one layer. This works for simple text
  # case with two adjecent boxes. Need to test more widely
  # Could make this conditional on geometry being a multipolygon
  rings <-
    rings %>%
    stringr::str_replace_all("\\[\\[\\[\\[", "[[[") %>%
    stringr::str_replace_all("\\]\\]\\]\\]", "]]]") %>%
    stringr::str_replace_all("\\]\\]\\],\\[\\[\\[", "]],[[")
  
  # Format the json and return
  glue::glue("{'(json_type)':(rings),'spatialReference':{'wkid':(crs)}}",
             .open = "(",
             .close = ")")
}

# Example
pol <- st_as_sf(st_as_sfc(st_bbox(st_as_sf(x = data.frame(x = c(58200.16, 74224.55, 58200.16, 74224.55),
                                                 y = c(6708224.65, 6708224.65, 6715024.86, 6715024.86)), coords = c("x", "y"), crs = 25833))))
sf_to_json(pol)
rm(pol)

#





# List parameters ----

# List parameters available via NVE's GridTimeSeries API

list_gts_parameters <- function(){
  
  # Libraries
  require(httr2)
  require(dplyr)
  
  # URL to download API
  url_api <- "http://gts.nve.no/api/GridTimeSeries/Themes/json"
  
  # Make request
  req <- request(base_url = url_api)
  
  # Run request
  resp <- req |> req_perform()
  
  # Collect results
  res <- resp |> resp_body_json()
  
  # Create data frame
  res <- bind_rows(res) %>%
    as.data.frame()
  
  # Return
  res
  
}

# Example
list_gts_parameters()

#



# List CellIndices for polygon ----

# Extract and list the CellIndex for each raster cell within a given polygon.

list_cellindex <- function(polygon){
  
  # polygon: polygon as json rings with spatialReference. Use sf_to_json() to convert from sf til json. Coordinate system: (EPSG: 25833)
  
  # Libraries
  require(httr2)
  require(lubridate)
  require(reshape2)
  require(dplyr)
  
  # URL to download API
  url_api <- "http://gts.nve.no/api/AreaTimeSeries/ByGeoJson"
  
  # Make request
  req <- request(url_api) |>
    req_body_json(list(Theme = "tm", StartDate = "2020-01-01", EndDate = "2020-01-01", Format = "json", Rings = polygon))

  # Run request
  resp <- req |> req_perform()
  
  # Collect results
  res <- resp |> resp_body_json()
  
  # Extract cellindices
  res <- bind_rows(res$CellTimeSeries) %>%
    as.data.frame()
  
  # Return
  res$CellIndex
  
}

# Example
pol <- st_as_sf(st_as_sfc(st_bbox(st_as_sf(x = data.frame(x = c(58200.16, 74224.55, 58200.16, 74224.55),
                                                          y = c(6708224.65, 6708224.65, 6715024.86, 6715024.86)), coords = c("x", "y"), crs = 25833))))

list_cellindex(polygon = sf_to_json(pol))
rm(pol)

#



# Download values at given coordinates ----

# Downloads and returns data for a point (coordinate) in a grid cell (coordinate system: (EPSG: 25833)). Time resolution is given by the environmental variable (env_layer) and start_date chosen. E.g. tm3h for "2023-12-01T06" is three hour data collected at 06:00.
download_cellwise_coords <- function(coords, env_layer, start_date, end_date, verbose = FALSE){
  
  # coords: A data frame with x and y coordinates (coordinate system: (EPSG: 25833))
  # env_layer: The quoted abbreviation for the environmental layer to download. E.g. Døgnnedbør = "rr", Temperatur =  "tm", Snødybde =  "sd", Snøtilstand = "lwc"
  # start_date, end_date: Date is specified like this: 'YYYY-MM-DD'. If querying three hour date, the hour is specified like this: format: 'YYYY-MM-DDT03'
  
  # Libraries
  require(httr2)
  require(lubridate)
  require(hms)
  require(reshape2)
  require(dplyr)
  
  # URL to download API
  url_api <- "http://gts.nve.no/api/MultiPointTimeSeries/ByMapCoordinateCsv"
  
  # Prepare coordiantes (remove any decimals and paste)
  coords$x <- round(coords$x)
  coords$y <- round(coords$y)
  coords <- paste(apply(X = coords, MARGIN = 1, FUN = function(x)paste(x, collapse = " ")), collapse = ", ")
  
  # Make request
  req <- request(url_api) |>
    req_body_json(list(Theme = env_layer, StartDate = start_date, EndDate = end_date, Format = "json", MapCoordinateCsv = coords))
  
  # Dry run
  if(verbose)
    req |> req_dry_run()
  
  # Run request
  resp <- req |> req_perform()
  
  # Collect results
  res <- resp |> resp_body_json()
  
  # Find time resolution
  timeres <- res$TimeResolution
  # Select step size for sequence of date and time
  datetime_steps <- switch(as.character(timeres),
                           "1440" = "days",
                           "180" = "3 hour",
                           "60" = "hour")
  
  # Make results into data frame
  # Data frame
  resdf <- bind_rows(lapply(res$CellTimeSeries, function(x) data.frame(x = x$X, y = x$Y, altitude = x$Altitude, cellindex = x$CellIndex, values = unlist(x$Data))))
  
  # NoDataValue
  nodata <- res$NoDataValue
  
  # Date and time
  datetime <- seq(dmy_hm(res$StartDate), dmy_hm(res$EndDate), by = datetime_steps)
  
  # Time
  time <- format(as.POSIXlt(as_hms(datetime)), "%H:%M")
  
  # Add variables
  resdf$date <- date(datetime)
  resdf$variable <- res$Theme
  resdf$unit <- res$Unit
  resdf$time <- time
  resdf$time_resolution_minutes <- res$TimeResolution
  
  # Replace NoDataValue by NA
  resdf[resdf$values == nodata]$values <- NA
  
  # Cast
  resdf <- dcast(resdf , formula = x + y + cellindex + altitude + date + time + time_resolution_minutes + unit ~ variable, value.var = "values")
  
  # Return
  resdf
}

# Example
coords <- data.frame(x = c(58200, 74224), y = c(6708224, 6715024))
download_cellwise_coords(coords = coords, env_layer = "tm", start_date = "2023-12-01", end_date = "2023-12-02")
download_cellwise_coords(coords = coords, env_layer = "tm3h", start_date = "2023-12-01T23", end_date = "2023-12-02T06")
download_cellwise_coords(coords = coords, env_layer = "tm1h", start_date = "2023-12-01T23", end_date = "2023-12-02T06")
rm(coords)

#



# Download cell wise values for polygon ----

# Downloads and returns data for each grid cell within a polygon (coordinate system: (EPSG: 25833)). Time resolution is given by the environmental variable (env_layer) and start_date chosen. E.g. tm3h for "2023-12-01T06" is three hour data collected at 06:00.

download_cellwise_polygon <- function(polygon, env_layer, start_date, end_date, verbose = FALSE){
  
  # env_layer: The quoted abbreviation for the environmental layer to download. E.g. Døgnnedbør = "rr", Temperatur =  "tm", Snødybde =  "sd", Snøtilstand = "lwc"
  # start_date, end_date: Date is specified like this: 'YYYY-MM-DD'. If querying three hour date, the hour is specified like this: format: 'YYYY-MM-DDT03'
  # polygon: polygon as json rings with spatialReference. Use sf_to_json() to convert from sf til json. Coordinate system: (EPSG: 25833)
  
  # Libraries
  require(httr2)
  require(lubridate)
  require(reshape2)
  require(dplyr)
  
  # URL to download API
  url_api <- "http://gts.nve.no/api/AreaTimeSeries/ByGeoJson"
  
  # Make request
  req <- request(url_api) |>
    req_body_json(list(Theme = env_layer, StartDate = start_date, EndDate = end_date, Format = "json", Rings = polygon))
  
  # Dry run
  if(verbose)
    req |> req_dry_run()
  
  # Run request
  resp <- req |> req_perform()
  
  # Collect results
  res <- resp |> resp_body_json()
  
  # Find time resolution
  timeres <- res$TimeResolution
  # Select step size for sequence of date and time
  datetime_steps <- switch(as.character(timeres),
                           "1440" = "days",
                           "180" = "3 hour",
                           "60" = "hour")
  
  # Make results into data frame
  # Data frame
  resdf <- bind_rows(lapply(res$CellTimeSeries, function(x) data.frame(altitude = x$Altitude, cellindex = x$CellIndex, values = unlist(x$Data))))
  
  # NoDataValue
  nodata <- res$NoDataValue
  
  # Date and time
  datetime <- seq(dmy_hm(res$StartDate), dmy_hm(res$EndDate), by = datetime_steps)
  
  # Time
  time <- format(as.POSIXlt(as_hms(datetime)), "%H:%M")
  
  # Add variables
  resdf$date <- date(datetime)
  resdf$variable <- res$Theme
  resdf$unit <- res$Unit
  resdf$time <- time
  resdf$time_resolution_minutes <- res$TimeResolution
  
  # Replace NoDataValue by NA
  resdf[resdf$values == nodata]$values <- NA
  
  # Cast
  resdf <- dcast(resdf , formula = cellindex + altitude + date + time + time_resolution_minutes + unit ~ variable, value.var = "values")
  
  # Return
  resdf
}

# Example
pol <- st_as_sf(st_as_sfc(st_bbox(st_as_sf(x = data.frame(x = c(58200.16, 74224.55, 58200.16, 74224.55),
                                                          y = c(6708224.65, 6708224.65, 6715024.86, 6715024.86)), coords = c("x", "y"), crs = 25833))))
download_cellwise_polygon(polygon = sf_to_json(pol), env_layer = "tm", start_date = "2023-12-01", end_date = "2023-12-01")
download_cellwise_polygon(polygon = sf_to_json(pol), env_layer = "tm1h", start_date = "2023-12-01T00", end_date = "2023-12-01T01")
rm(pol)

#



# Download cell wise values for CellIndex ----

# Downloads and returns data for each grid cell identified by their CellIndex (the index for raster cells in GridTimeSeries (GTS) data fra NVE). Time resolution is given by the environmental variable (env_layer) and start_date chosen. E.g. tm3h for "2023-12-01T06" is three hour data collected at 06:00.

# This is faster than download_cellwise_polygon.

download_cellwise_cellindex <- function(cellindex, env_layer, start_date, end_date, verbose = FALSE){
  
  # cellindex: en vector med celleindex for cellene som skal lastes GTS data fra
  # env_layer: The quoted abbreviation for the environmental layer to download. E.g. Døgnnedbør = "rr", Temperatur =  "tm", Snødybde =  "sd", Snøtilstand = "lwc"
  # start_date, end_date: Date is specified like this: 'YYYY-MM-DD'. If querying three hour date, the hour is specified like this: format: 'YYYY-MM-DDT03'
  
  # Libraries
  require(httr2)
  require(lubridate)
  require(reshape2)
  require(dplyr)
  
  # URL to download API
  url_api <- "http://gts.nve.no/api/AreaTimeSeries/ByCellIndexCsv"
  
  
  # Prepare cellindices
  cellindex <- paste(cellindex, collapse = ",")
  
  
  # Make request
  req <- request(url_api) |>
    req_body_json(list(Theme = env_layer, StartDate = start_date, EndDate = end_date, Format = "json", CellIndexCsv = cellindex))
  
  # Dry run
  if(verbose)
    req |> req_dry_run()
  
  # Run request
  resp <- req |> req_perform()
  
  # Collect results
  res <- resp |> resp_body_json()
  
  # Find time resolution
  timeres <- res$TimeResolution
  # Select step size for sequence of date and time
  datetime_steps <- switch(as.character(timeres),
                           "1440" = "days",
                           "180" = "3 hour",
                           "60" = "hour")
  
  # Make results into data frame
  # Data frame
  resdf <- bind_rows(lapply(res$CellTimeSeries, function(x) data.frame(altitude = x$Altitude, cellindex = x$CellIndex, values = unlist(x$Data))))
  
  # NoDataValue
  nodata <- res$NoDataValue
  
  # Date and time
  datetime <- seq(dmy_hm(res$StartDate), dmy_hm(res$EndDate), by = datetime_steps)
  
  # Time
  time <- format(as.POSIXlt(as_hms(datetime)), "%H:%M")
  
  # Add variables
  resdf$date <- date(datetime)
  resdf$variable <- res$Theme
  resdf$unit <- res$Unit
  resdf$time <- time
  resdf$time_resolution_minutes <- res$TimeResolution
  
  # Replace NoDataValue by NA
  resdf[resdf$values == nodata]$values <- NA
  
  # Cast
  resdf <- dcast(resdf , formula = cellindex + altitude + date + time + time_resolution_minutes + unit ~ variable, value.var = "values")
  
  # Return
  return(resdf)
  
  
  # Make results into data frame
  # NoDataValue
  nodata <- res$NoDataValue
  # Dates
  dateseq <- seq(date(dmy_hm(res$StartDate)), date(dmy_hm(res$EndDate)), by = 1)
  # Data
  # Names in list
  resnm <- names(res$CellTimeSeries[[1]])
  # Length of names in list + length of data -1
  resnml <- length(resnm) + length(res$CellTimeSeries[[1]]$Data) - 1
  # Names in list with names of days of data
  resnm <- c(resnm[-length(resnm)], paste0(resnm[length(resnm)], 1:length(res$CellTimeSeries[[1]]$Data)))
  # Make results into data frame
  resdf <- melt(do.call(bind_rows, lapply(res$CellTimeSeries, function(df) as.data.frame(matrix(unlist(df), ncol=resnml, dimnames = list(NULL, resnm))))), id.vars = c("CellIndex", "Altitude"), value.name = env_layer)
  # Remove "variable"
  resdf <- select(resdf, -variable)
  # Arrange
  resdf <- arrange(resdf, CellIndex)
  # Add dates
  resdf$date <- dateseq
  # Replace NoDataValue by NA
  resdf[resdf[, env_layer] == nodata, env_layer] <- NA
  
  # Return
  resdf
}

# Example
pol <- st_as_sf(st_as_sfc(st_bbox(st_as_sf(x = data.frame(x = c(58200.16, 74224.55, 58200.16, 74224.55),
                                                          y = c(6708224.65, 6708224.65, 6715024.86, 6715024.86)), coords = c("x", "y"), crs = 25833))))
cells <- list_cellindex(polygon = sf_to_json(pol))
download_cellwise_cellindex(cellindex = cells, env_layer = "tm", start_date = "2023-12-01", end_date = "2023-12-01")
download_cellwise_cellindex(cellindex = cells, env_layer = "tm3h", start_date = "2023-12-01T06", end_date = "2023-12-01T09")
rm(pol, cells)

#



# Download aggregated values for polygon ----

# Downloads and returns data aggregated for the chosen time resolution over grid cells within a polygon (coordinate system: (EPSG: 25833)). Available methods for aggregating are sum, min, max, avg and median. Time resolution is given by the environmental variable (env_layer) and start_date chosen. E.g. tm3h for "2023-12-01T06" is three hour data collected at 06:00.

download_aggregated_polygon <- function(polygon, env_layer, start_date, end_date, method, verbose = FALSE){
  
  # env_layer: The quoted abbreviation for the environmental layer to download. E.g. Døgnnedbør = "rr", Temperatur =  "tm", Snødybde =  "sd", Snøtilstand = "lwc"
  # start_date, end_date: Date is specified like this: 'YYYY-MM-DD'. If querying three hour date, the hour is specified like this: format: 'YYYY-MM-DDT03'
  # method: The method for aggregating values ("sum", "min", "max", "avg" or "median")
  # polygon: polygon as json rings with spatialReference. Use sf_to_json() to convert from sf til json. Coordinate system: (EPSG: 25833)
  
  # Libraries
  require(httr2)
  require(lubridate)
  require(reshape2)
  require(dplyr)
  
  
  # URL to download API
  url_api <- "http://gts.nve.no/api/AggregationTimeSeries/ByGeoJson"
  
  # Make request
  req <- request(url_api) |>
    req_body_json(list(Theme = env_layer, StartDate = start_date, EndDate = end_date, Format = "json", Method = method, Rings = polygon))
  
  # Dry run
  if(verbose)
    req |> req_dry_run()
  
  # Run request
  resp <- req |> req_perform()
  
  # Collect results
  res <- resp |> resp_body_json()
  
  # Make results into data frame
  # NoDataValue
  nodata <- res$NoDataValue
  # Dates
  dateseq <- seq(date(dmy_hm(res$StartDate)), date(dmy_hm(res$EndDate)), by = 1)
  # Time
  time <- str_split(res$StartDate, " ")[[1]][2]
  # Data frame
  resdf <- data.frame(date = dateseq, variable = res$Theme, unit = res$Unit, time = time, time_resolution_minutes = res$TimeResolution, values = unlist(res$Data), method = res$Method)
  
  # Replace NoDataValue by NA
  resdf[resdf$values == nodata]$values <- NA
  
  # Cast
  resdf <- dcast(resdf , formula = date + time + time_resolution_minutes + unit ~ variable + method, value.var = "values")
  
  # Return
  resdf
}

# Example
pol <- st_as_sf(st_as_sfc(st_bbox(st_as_sf(x = data.frame(x = c(58200.16, 74224.55, 58200.16, 74224.55),
                                                          y = c(6708224.65, 6708224.65, 6715024.86, 6715024.86)), coords = c("x", "y"), crs = 25833))))
download_aggregated_polygon(polygon = sf_to_json(pol), env_layer = "tm", start_date = "2023-12-01", end_date = "2023-12-02", method = "avg")
download_aggregated_polygon(polygon = sf_to_json(pol), env_layer = "tm1h", start_date = "2023-12-01T06", end_date = "2023-12-01T06", method = "avg")
rm(pol)

#

# Download aggregated values for CellIndex ----

# Downloads and returns data aggregated for the chosen time resolution over grid cells identified by their CellIndex. Available methods for aggregating are sum, min, max, avg and median. Time resolution is given by the environmental variable (env_layer) and start_date chosen. E.g. tm3h for "2023-12-01T06" is three hour data collected at 06:00.

# This is faster than download_aggregated_polygon.

download_aggregated_cellindex <- function(cellindex, env_layer, start_date, end_date, method, verbose = FALSE){
  
  # env_layer: The quoted abbreviation for the environmental layer to download. E.g. Døgnnedbør = "rr", Temperatur =  "tm", Snødybde =  "sd", Snøtilstand = "lwc"
  # start_date, end_date: Date is specified like this: 'YYYY-MM-DD'. If querying three hour date, the hour is specified like this: format: 'YYYY-MM-DDT03'
  # method: The method for aggregating values ("sum", "min", "max", "avg" or "median")
  # polygon: polygon as json rings with spatialReference. Use sf_to_json() to convert from sf til json. Coordinate system: (EPSG: 25833)
  
  # Libraries
  require(httr2)
  require(lubridate)
  require(reshape2)
  require(dplyr)
  
  # URL to download API
  url_api <- "http://gts.nve.no/api/AggregationTimeSeries/ByCellIndexCsv"
  
  # Prepare cellindices
  cellindex <- paste(cellindex, collapse = ",")
  
  # Make request
  req <- request(url_api) |>
    req_body_json(list(Theme = env_layer, StartDate = start_date, EndDate = end_date, Format = "json", Method = method, CellIndexCsv = cellindex))
  
  # Dry run
  if(verbose)
    req |> req_dry_run()
  
  # Run request
  resp <- req |> req_perform()
  
  # Collect results
  res <- resp |> resp_body_json()
  
  # Make results into data frame
  # NoDataValue
  nodata <- res$NoDataValue
  # Dates
  dateseq <- seq(date(dmy_hm(res$StartDate)), date(dmy_hm(res$EndDate)), by = 1)
  # Time
  time <- str_split(res$StartDate, " ")[[1]][2]
  # Data frame
  resdf <- data.frame(date = dateseq, variable = res$Theme, unit = res$Unit, time = time, time_resolution_minutes = res$TimeResolution, values = unlist(res$Data), method = res$Method)
  
  # Replace NoDataValue by NA
  resdf[resdf$values == nodata]$values <- NA
  
  # Cast
  resdf <- dcast(resdf , formula = date + time + time_resolution_minutes + unit ~ variable + method, value.var = "values")
  
  # Return
  resdf
}

# Example
pol <- st_as_sf(st_as_sfc(st_bbox(st_as_sf(x = data.frame(x = c(58200.16, 74224.55, 58200.16, 74224.55),
                                                          y = c(6708224.65, 6708224.65, 6715024.86, 6715024.86)), coords = c("x", "y"), crs = 25833))))
cells <- list_cellindex(polygon = sf_to_json(pol))
download_aggregated_cellindex(cellindex = cells, env_layer = "tm", start_date = "2023-12-01", end_date = "2023-12-02", method = "avg")
download_aggregated_cellindex(cellindex = cells, env_layer = "tm3h", start_date = "2023-12-01T06", end_date = "2023-12-02T06", method = "avg")
rm(pol, cells)

#


