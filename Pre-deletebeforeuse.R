library(shiny)
require(rgdal)
require(rgeos)
require(zip)
require(dismo)
require(raster)
require(matrixStats)

setwd("S:/COS/PyroGeog/amartinez/Ranking/CriteriaData")
setwd("D:/CriteriaData")

fire.perim <- readOGR("UnburnedIsland/mtbs_perims_1984_2014.shp", stringsAsFactors = F)
fire.perim@data$FireDesc <- paste0(fire.perim@data$Fire_Name, " (",substr(fire.perim@data$Fire_ID, 1, 2), " - ", fire.perim@data$Year, ") ", fire.perim@data$Fire_ID)
fire.perim@data$StartDate <- paste0(fire.perim@data$Year, "-", fire.perim@data$StartMonth, "-", fire.perim@data$StartDay)

firelist <- fire.perim@data
firelist$Year <- as.integer(firelist$Year)

pnw <- readOGR("PNW/pnw.shp")
pnw <- spTransform(pnw, proj4string(fire.perim))
pnw@bbox <- matrix(data = c(-118549.3, 4649251, 993162.5, 5445738), nr = 2, nc = 2, dimnames = list(c("x", "y"), c("min", "max")))

unb.isl <- readOGR("UnburnedIsland/unburned_areas.shp", stringsAsFactors = F)

age <- raster("StandAge/StandAge.clip.tif")
fcc <- readOGR("Infrastructure/Intermediates/FCC_Point.shp")
nps <- readOGR("Infrastructure/Intermediates/NPS_Point.shp")
usfs <- readOGR("Infrastructure/Intermediates/USFS_Point.shp")
blm.p <- readOGR("Infrastructure/Intermediates/BLM_Point.shp", stringsAsFactors = F)
blm.l <- readOGR("Infrastructure/Intermediates/BLM_Line.shp", stringsAsFactors = F)
hab <- readOGR("CriticalHabitat/CriticalHabitat.shp")
inv <- readOGR("Invasive/Invasive.shp")
cvr <- raster("LandCover/LandCoverSA.tif")

thresh <- data.frame(Variable = as.character(c("CritHab", "Infrstr", "Invasiv", "Isolatn", "LandCvr", "Seed", "StndAge", "Size")),
                     True = c(1, 3, 0, 300, 0.1, 300, 250, 9000),
                     False = c(0, 0, 0.4, 0, 0.5, 0, 20, 0))


##
rm(list = objects()[- which(objects() %in% c("unb", "fire.perim", "pnw", "firelist", 
                                             "age", "blm.l", "blm.p", "fcc", "inv", "usfs", 
                                             "nps", "hab", "cvr"))])
######
# Hambre Mtn (small)
fire.sel <- fire.perim[fire.perim$Fire_ID %in% "WA4628912040319841017",]
unb.sel <- unb.isl[unb/isl$fire_id %in% "WA4628912040319841017",]

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

####
# FCC = 1
fcc@data$w <- rep(1, times = nrow(fcc))
usfs@data$w <- rep(1, times = nrow(usfs))
nps@data$w <- rep(1, times = nrow(nps))
blm.l@data$w <- rep(0.5, times = nrow(blm.l))

blm.p@data$STRCT_TYPE[is.na(blm.p@data$STRCT_TYPE)] <- blm.p@data$STRCT_TY_1[is.na(blm.p@data$STRCT_TYPE)]
blm.p@data$STRCT_TYPE <- toupper(blm.p@data$STRCT_TYPE)
blm.p.scores <- read.csv("Infrastructure/Intermediates/blm_p_scores.csv", stringsAsFactors = F)

blm.p@data$w <- merge(blm.p@data, blm.p.scores, by = "STRCT_TYPE", all.x = T)$Score



###
head(a)

blm.l@data$STRCT_TYPE[is.na(blm.l@data$STRCT_TYPE)] <- blm.l@data$STRCT_TY_1[is.na(blm.l@data$STRCT_TYPE)]
blm.l@data$STRCT_TYPE <- toupper(blm.l@data$STRCT_TYPE)
b <- unique(blm.l@data$STRCT_TYPE)

blm.p.lk <- data.frame(STRCT_TYPE = names(table(blm.p@data$STRCT_TYPE)),
                w = c(.5, .5, .5, 1, 1, 1, .5, .5,
                      .5, .5, .5, .5, .5, .5, .5, .5,
                      .5, .5, .5, .5, .5, .5, .5, .5,
                      1, .5, .5, 1, 1, .5, .5, .5,
                      .5, .5, .5, .5, .5, .5, .5, .5, 
                      .5, 1))
a <- merge(blm.p@data, blm.p.lk, by = "STRCT_TYPE", all.x = T)
b1 <- blm.p@data[is.na(blm.p@data$STRCT_TYPE),]
b2 <- blm.p@data[is.na(blm.p@data$STRCT_TY_1),]
b3 <- blm.p@data[is.na(blm.p@data$STRCT_TY_1) & is.na(blm.p@data$STRCT_TYPE),]
table(blm.l@data$STRCT_TY_1)


#####

library(diagram)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
openplotmat()

elpos<-coordinates(c(1,3,8 ))
##draw arrows from each row to next row
treearrow(from=elpos[1,],to=elpos[c(2:4),],lwd=4, arr.side = 0)  
treearrow(from=elpos[2,],to=elpos[5:7,],lwd=4, arr.side = 0)
treearrow(from=elpos[3,],to=elpos[8,],lwd=4, arr.side = 0)
treearrow(from=elpos[4,],to=elpos[9:12,],lwd=4, arr.side = 0)


##create a generic 3-lined label for each textbox
label.1 <- c("Refugia value", "Quality habitat", "Infrastructure", "Unique habitat",
             "Size", "Critical Habitat", "Invasive\nspecies", "Infrastructure",
             "Seedling", "Land Cover", "Isolation", "Stand age")
weights <- c(1,1,1,2,3,2,1,4,5,4,5)
label.2 <- c("\nFinal Value", paste0("\nWt: ", weights))
label.3 <- paste0(label.1, label.2)

##plot text boxes
for (i in 1:12) textrect(elpos[i,],radx=0.055,rady=0.05,lab=label.3[i], cex = 0.7, shadow.size = 0)

###
ui <- fluidPage(
  checkboxGroupInput(inputId="test", label="Test", choices=1:4, inline = TRUE)
)
server <- function(input, output) {
  
}
shinyApp(ui = ui, server = server)

