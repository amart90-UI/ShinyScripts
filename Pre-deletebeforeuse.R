library(rgdal)

setwd("S:/COS/PyroGeog/amartinez/Ranking/CriteriaData")

fire.perim <- readOGR("UnburnedIsland/mtbs_perims_1984_2014.shp", stringsAsFactors = F)
fire.perim@data$FireDesc <- paste0(fire.perim@data$Fire_Name, " (",substr(fire.perim@data$Fire_ID, 1, 2), " - ", fire.perim@data$Year, ") ", fire.perim@data$Fire_ID)
fire.perim@data$StartDate <- paste0(fire.perim@data$Year, "-", fire.perim@data$StartMonth, "-", fire.perim@data$StartDay)

firelist <- fire.perim@data
firelist$Year <- as.numeric(firelist$Year)

pnw <- readOGR("PNW/pnw.shp")
pnw <- spTransform(pnw, proj4string(fire.perim))
pnw@bbox <- matrix(data = c(-118549.3, 4649251, 993162.5, 5445738), nr = 2, nc = 2, dimnames = list(c("x", "y"), c("min", "max")))

unb <- readOGR("UnburnedIsland/unburned_areas.shp", stringsAsFactors = F)


##
rm(list = objects()[- which(objects() %in% c("unb", "fire.perim", "pnw"))])

######

firelist$FireDesc[1]
