library(shiny)
require(rgdal)
require(rgeos)
require(zip)
require(dismo)
require(raster)
require(matrixStats)

setwd("S:/COS/PyroGeog/amartinez/Ranking/CriteriaData")
setwd("D:/CriteraData")

fire.perim <- readOGR("UnburnedIsland/mtbs_perims_1984_2014.shp", stringsAsFactors = F)
fire.perim@data$FireDesc <- paste0(fire.perim@data$Fire_Name, " (",substr(fire.perim@data$Fire_ID, 1, 2), " - ", fire.perim@data$Year, ") ", fire.perim@data$Fire_ID)
fire.perim@data$StartDate <- paste0(fire.perim@data$Year, "-", fire.perim@data$StartMonth, "-", fire.perim@data$StartDay)

firelist <- fire.perim@data
firelist$Year <- as.integer(firelist$Year)

pnw <- readOGR("PNW/pnw.shp")
pnw <- spTransform(pnw, proj4string(fire.perim))
pnw@bbox <- matrix(data = c(-118549.3, 4649251, 993162.5, 5445738), nr = 2, nc = 2, dimnames = list(c("x", "y"), c("min", "max")))

unb <- readOGR("UnburnedIsland/unburned_areas.shp", stringsAsFactors = F)

age <- raster("StandAge/StandAge.clip.tif")
fcc <- readOGR("Infrastructure/Intermediates/FCC_Point.shp")
nps <- readOGR("Infrastructure/Intermediates/NPS_Point.shp")
usfs <- readOGR("Infrastructure/Intermediates/USFS_Point.shp")
blm.p <- readOGR("Infrastructure/Intermediates/BLM_Point.shp")
blm.l <- readOGR("Infrastructure/Intermediates/BLM_Line.shp")
hab <- readOGR("CriticalHabitat/CriticalHabitat.shp")
inv <- readOGR("Invasive/Invasive.shp")
cvr <- raster("LandCover/LandCoverSA.tif")


##
rm(list = objects()[- which(objects() %in% c("unb", "fire.perim", "pnw", "firelist", 
                                             "age", "blm.l", "blm.p", "fcc", "inv", "usfs", 
                                             "nps", "hab", "cvr"))])

######
# Hambre Mtn (small)
fire.sel <- fire.perim[fire.perim$Fire_ID %in% "WA4628912040319841017",]
unb.sel <- unb[unb$fire_id %in% "WA4628912040319841017",]

# Table Mtn (big)
fire.sel <- fire.perim[fire.perim$Fire_ID %in% "WA4724812058320120908",]
unb.sel <- unb[unb$fire_id %in% "WA4724812058320120908",]

# WA4803111998320120916 goat
fire.sel <- fire.perim[fire.perim$Fire_ID %in% "ID4452811445820130720",]
unb.sel <- unb[unb$fire_id %in% "ID4452811445820130720",]

save(list = objects(), file = "D:/antho/Google Drive/UI-Drive/Refugia/SpData")
save(list = objects(), file = "S:/COS/PyroGeog/amartinez/Ranking/CriteriaData/SpData")

load("S:/COS/PyroGeog/amartinez/Ranking/CriteriaData/SpData")
load("D:/antho/Google Drive/UI-Drive/Refugia/SpData")



writeRaster(age, "StandAge/StandAge.clip.tif")
writeOGR(fcc, dsn="Infrastructure/Intermediates/FCC_Point", layer="FCC_Point", driver="ESRI Shapefile")
writeOGR(nps, dsn="Infrastructure/Intermediates/NPS_Point", layer="NPS_Point", driver="ESRI Shapefile")
writeOGR(usfs, dsn="Infrastructure/Intermediates/USFS_Point", layer="USFS_Point", driver="ESRI Shapefile")
writeOGR(blm.p, dsn="Infrastructure/Intermediates/BLM_Point", layer="BLM_Point", driver="ESRI Shapefile")
writeOGR(blm.l, dsn="Infrastructure/Intermediates/BLM_Line", layer="BLM_Line", driver="ESRI Shapefile")
writeOGR(hab, dsn="CriticalHabitat/CriticalHabitat", layer="CriticalHabitat", driver="ESRI Shapefile")
writeOGR(inv, dsn="Invasive/Invasive", layer="Invasive", driver="ESRI Shapefile")
writeRaster(cvr, "LandCover/LandCoverSA.tif")
writeOGR(unb, dsn="UnburnedIsland/unburned_areas", layer="unburned_areas", driver="ESRI Shapefile")
writeOGR(fire.perim, dsn="UnburnedIsland/mtbs_perims_1984_2014", layer="mtbs_perims_1984_2014", driver="ESRI Shapefile")
writeOGR(pnw, dsn="pnw", layer="PNW", driver="ESRI Shapefile")

