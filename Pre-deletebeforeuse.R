library(rgdal)
setwd("C:/Users/PyroGeo/Refugia/Ranking") # school computer
pnw.map <- readOGR("Output/pnw.shp")
fire.perim <- readOGR("Datasets/Unburned Island/mtbs_perims_1984_2014_clp_utm_farea.shp", stringsAsFactors = F)
fire.perim@data$FireDesc <- paste(fire.perim@data$Fire_Name, substr(fire.perim@data$Fire_ID, 1, 2), fire.perim@data$Year, sep = " - ")
fire.perim@data$StartDate <- paste0(fire.perim@data$Year, "-", fire.perim@data$StartMonth, "-", fire.perim@data$StartDay)
pnw.map <- spTransform(pnw.map, proj4string(fire.perim))
pnw.map@bbox <- matrix(data = c(-118549.3, 4649251, 993162.5, 5445738), nr = 2, nc = 2, dimnames = list(c("x", "y"), c("min", "max")))

firelist <- fire.perim@data
firelist$Year <- as.numeric(firelist$Year)

######
library(rgdal)

setwd("S:/COS/PyroGeog/amartinez/Ranking/CriteriaData")
pnw <- readOGR("PNW/pnw.shp")
pnw <- spTransform(pnw, proj4string(fire.perim))
pnw@bbox <- matrix(data = c(-118549.3, 4649251, 993162.5, 5445738), nr = 2, nc = 2, dimnames = list(c("x", "y"), c("min", "max")))

fire.perim <- readOGR("UnburnedIsland/mtbs_perims_1984_2014.shp")
fire.perim@data$FireDesc <- paste(fire.perim@data$Fire_Name, substr(fire.perim@data$Fire_ID, 1, 2), fire.perim@data$Year, sep = " - ")
fire.perim@data$StartDate <- paste0(fire.perim@data$Year, "-", fire.perim@data$StartMonth, "-", fire.perim@data$StartDay)

firelist <- fire.perim@data
firelist$Year <- as.numeric(firelist$Year)

unb <- readOGR("UnburnedIsland/unburned_areas.shp")