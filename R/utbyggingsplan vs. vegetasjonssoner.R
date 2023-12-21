library(sf)
library(terra)
library(dplyr)

buildPlan <- st_read("P:/823001_18_metodesats_analyse_23_26_roos/Planlagt-Utbygging2023_0000_Norge_25833_GPKG.gpkg")

veg_zones <- terra::rast("P:/823001_18_metodesats_analyse_23_26_roos/Naturindeks_N50_vegetasjonssoner_25m.tif")


buildPlan <- buildPlan %>% st_transform(crs = st_crs(veg_zones))


#val <- terra::extract(veg_zones, vect(buildPlan))

#write.table(val, file="P:/823001_18_metodesats_analyse_23_26_roos/val.txt")
val <- read.table("P:/823001_18_metodesats_analyse_23_26_roos/val.txt", header=T)

N <- 
  val %>% 
  group_by(ID) %>%
  summarize(N = length(b1))

N4 <- 
  val %>% 
  group_by(ID) %>%
  summarize(N = length(b1[b1==4]))

N3 <- 
  val %>% 
  group_by(ID) %>%
  summarize(N = length(b1[b1==3]))

N2 <- 
  val %>% 
  group_by(ID) %>%
  summarize(N = length(b1[b1==2]))

N1 <- 
  val %>% 
  group_by(ID) %>%
  summarize(N = length(b1[b1==1]))

N0 <- 
  val %>% 
  group_by(ID) %>%
  summarize(N = length(b1[b1==0]))


buildPlan$ratio4 <- N4$N/N$N
buildPlan$ratio3 <- N3$N/N$N
buildPlan$ratio2 <- N2$N/N$N
buildPlan$ratio1 <- N1$N/N$N
buildPlan$ratio0 <- N0$N/N$N
buildPlan$N <- N$N
buildPlan$N1 <- N1$N
buildPlan$N2 <- N2$N
buildPlan$N3 <- N3$N
buildPlan$N4 <- N4$N

# number of cells with building plans
sum(buildPlan$N1)
sum(buildPlan$N2)
sum(buildPlan$N3)
sum(buildPlan$N4)
sum(sum(buildPlan$N1),
    sum(buildPlan$N2),
    sum(buildPlan$N3),
    sum(buildPlan$N4)
    )

# number of cells in different vegetation zones
highalp <- cells(veg_zones, 4)
midalp <- cells(veg_zones, 3)
lowalp <- cells(veg_zones, 2)
lowland <- cells(veg_zones, 1)

# ratio of cells in vegetation zones with a building plan
sum(buildPlan$N4)/length(highalp$b1)*100
sum(buildPlan$N3)/length(midalp$b1)*100
sum(buildPlan$N2)/length(lowalp$b1)*100
sum(buildPlan$N1)/length(lowland$b1)*100
sum(sum(buildPlan$N1),
    sum(buildPlan$N2),
    sum(buildPlan$N3),
    sum(buildPlan$N4)
    )/
  sum(length(highalp$b1),length(midalp$b1),length(lowalp$b1),length(lowland$b1))*100


