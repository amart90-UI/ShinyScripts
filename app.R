library(shiny)

#############################################################

ui <- fluidPage(
  p("Select a fire below.  You may filter the list by the firest sart location, year or MTBS fire ID"),
  checkboxGroupInput(inputId = "inStateGroup", label = "Input state",
                     choices = c("WA", "OR", "ID", "Other"), 
                     selected = c("WA", "OR", "ID", "Other")),
  sliderInput(inputId = "inYearSlide", label = "Select year range", min = 1984, max = 2014, value = c(1984, 2014), sep = ""),
  textInput(inputId = "inNameText", label = "Enter FireName"),
  textInput(inputId = "inIdText", label = "Enter MTBS Fire ID"),
  selectInput(inputId = "inSelect", label = "Select input", choices = firelist$FireDesc),
  fluidRow(
    column(12,
           tableOutput('table')
    )
  ),
  plotOutput(outputId = "fireplot"),
  actionButton(inputId = "do", label = "Click Me"),
  selectInput(inputId = "inCrit", label = "Which criteria to color by", 
              choices = c("Size", "Isolation", "Seedling", "Infrastructure", "Stand Age", "Critical Habitat", "Invasive")),
  withSpinner(plotOutput(outputId = "fireplotzoom"), type = 5, color = "#ccd1c8"),
  downloadButton(outputId = "downloadFire", label = 'Download fire perimeter'),
  #withSpinner(downloadButton(outputId = "downloadUnb", label = 'Download unburned island'), type = 5, color = "#961515")
  downloadButton(outputId = "downloadUnb", label = 'Download unburned island')
)

server <- function(input, output, session) {
  require(rgdal)
  require(rgeos)
  require(zip)
  require(dismo)
  require(raster)
  require(matrixStats)
  #load("D:/antho/Google Drive/UI-Drive/Refugia/SpData")
  
  observe({
    x <- input$inStateGroup
    if("Other" %in% x){
      x <- c(x[-which(x == "Other")], "CA", "MT", "NV", "UT", "WY")
    }
    y <- input$inYearSlide[1]:input$inYearSlide[2]
    z <- ifelse(is.null(input$inIdText),firelist$Fire_ID, toupper(input$inIdText))
    zz <- ifelse(is.null(input$inIdText),firelist$Fire_Name, toupper(input$inNameText))
    FireChoices <- firelist$FireDesc[substr(firelist$Fire_ID, 1, 2) %in% x 
                                     & firelist$Year %in% y 
                                     & firelist$Fire_ID %in% firelist$Fire_ID[grep(z, firelist$Fire_ID)]
                                     & firelist$Fire_Name %in% firelist$Fire_Name[grep(zz, firelist$Fire_Name)]]
    # Can also set the label and select items
    updateSelectInput(session, "inSelect",
                      label = paste("Select input label (", length(FireChoices), ")"),
                      choices = FireChoices
    )
  })
  output$fireplot <- renderPlot({
    plot(pnw)
    plot(fire.perim[fire.perim@data$FireDesc == input$inSelect,], col = "red", border = "red", lwd = 3, add = T)
  })
  output$table <- renderTable(firelist[firelist$FireDesc == input$inSelect,c("Fire_Name", "Year", "Acres", "StartDate", "Fire_ID")])
  fire <- eventReactive(input$do, {input$inSelect})
  fire.id <- eventReactive(input$do, {unique(fire.perim$Fire_ID[fire.perim@data$FireDesc %in% fire()])})
  fire.sel <- eventReactive(input$do, {fire.perim[fire.perim$Fire_ID %in% fire.id(),]})
  unb.sel <- eventReactive(input$do, {unb[unb$fire_id %in% fire.id(),]})
  
  size <- eventReactive(input$do, {Size(unb.sel())})
  isol <- eventReactive(input$do, {Isolation(unb.sel(), fire.sel())})
  seed <- eventReactive(input$do, {Seedling(unb.sel(), fire.sel())})
  infra <- eventReactive(input$do, {Infrastructure(unb.sel(), fire.sel())})
  age <- eventReactive(input$do, {StandAge(unb.sel(), fire.sel())})
  crithab <- eventReactive(input$do, {CritHabitat(unb.sel())})
  invas <- eventReactive(input$do, {Invasive(unb.sel(), fire.sel())})
  
  col <- reactive({
    if(input$inCrit == "Size"){
      Col(unb.sel(), unb.sel.tab()$Size)
    } else if(input$inCrit == "Isolation"){
      Col(unb.sel(), unb.sel.tab()$Isolatn)
    } else if(input$inCrit == "Seedling"){
      Col(unb.sel(), unb.sel.tab()$Seed)
    } else if(input$inCrit == "Infrastructure"){
      Col(unb.sel(), unb.sel.tab()$Infrstr)
    } else if(input$inCrit == "Stand Age"){
      Col(unb.sel(), unb.sel.tab()$StndAge)
    } else if(input$inCrit == "Critical Habitat"){
      Col(unb.sel(), unb.sel.tab()$CritHab)
    } else if(input$inCrit == "Invasive"){
      Col(unb.sel(), unb.sel.tab()$Invasiv)
    }
  })
  
  unb.sel.tab <- eventReactive(input$do, {
    data.frame(unb.sel()@data, Size = size(), Isolatn = isol(), Seed = seed(),
               Infrstr = infra(), StndAge = age(), CritHab = crithab(), Invasiv = invas())})
  
  unb.sel.app <- eventReactive(input$downloadFire, {SpatialPolygonsDataFrame(unb.sel(), data = unb.sel.tab())})
  
  
  #output$fireplotzoom <- renderPlot({
  #  plot(fire.sel(),)
  #  plot(unb.sel(), add = T, col = col(), border = col())
  #})
  
  clk <- reactiveValues(default = 0)
  observeEvent(input$do, {
    clk$default <- input$do
  })
  
  #fplot <- plot(fire.perim[fire.perim@data$FireDesc == input$inSelect,])
  fplot <-  eventReactive({
    input$do
    input$inSelect
    input$inCrit
  }, {
    plot(fire.perim[fire.perim@data$FireDesc == input$inSelect,])
    if(clk$default > 0) plot(unb.sel(), add = T, col = col(), border = col())
  }, ignoreNULL = F)
  
  output$fireplotzoom <- renderPlot({fplot()})
  
  #output$fireplotzoom <- renderPlot({
  #  plot(fire.perim[fire.perim@data$FireDesc == input$inSelect,])
  #  if(input$do >0) plot(unb.sel(), add = T, col = col(), border = col())
  #})}, ignoreNULL = F))
  
  #output$fireplotzoom <- if(!is.null(fire.sel)) renderPlot({
  #  plot(fire.sel(),)
  #  plot(unb.sel(), add = T, col = col(), border = col())
  #})
  
  output$downloadFire <- downloadHandler(
    filename = 'fire_perim.zip',
    content = function(file) {
      if (length(Sys.glob("fire_perim.*"))>0){
        file.remove(Sys.glob("fire_perim.*"))
      }
      writeOGR(fire.sel(), dsn="fire_perim.shp", layer="fire_perim", driver="ESRI Shapefile")
      zip(zipfile='fire_perim.zip', files=Sys.glob("fire_perim.*"))
      file.copy("fire_perim.zip", file)
      if (length(Sys.glob("fire_perim.*"))>0){
        file.remove(Sys.glob("fire_perim.*"))
      }
    }
  )
  
  output$downloadUnb <- downloadHandler(
    filename = 'unb_isl.zip',
    content = function(file) {
      if (length(Sys.glob("unb_isl.*"))>0){
        file.remove(Sys.glob("unb_isl.*"))
      }
      writeOGR(unb.sel.app(), dsn="unb_isl.shp", layer="unb_isl", driver="ESRI Shapefile")
      write.csv(unb.sel.tab(), file =  "unb_isl.csv")
      zip(zipfile='unb_isl.zip', files=Sys.glob("unb_isl.*"))
      file.copy("unb_isl.zip", file)
      if (length(Sys.glob("unb_isl.*"))>0){
        file.remove(Sys.glob("unb_isl.*"))
      }
    }
  )
  
  
  # Define Functions #
  #Define Size function
  Size <- function(unb){
    require(rgeos)
    # Calculate size of unburned islands
    size.unb <- gArea(unb, byid = T)
    return(size.unb)
  }
  
  #Define Isolation function
  Isolation <- function(unb, fire.perim){
    # Load packages
    require(rgdal)
    require(rgeos)
    require(matrixStats)
    
    # Calculate distance to fire perimeter for each UI
    s.dist.u <- as.matrix(gDistance(unb, byid=T))
    diag(s.dist.u) <- NA
    s.min.u <- rowMins(s.dist.u, na.rm = T)
    
    
    # Calculate distance to nearest UI for each UI
    s.min.p <- as.numeric(gDistance(unb, as(fire.perim, "SpatialLines"), byid=T))
    max(s.min.u)
    # Calculate distance to nearest live tree edge
    s.min <- as.matrix(cbind(s.min.u, s.min.p))
    s.min <- rowMins(as.matrix(s.min))
    
    return(s.min)
  }
  
  # Define Seedling function (7.00 min)
  Seedling <- function(unb, fire.perim) {
    # Load packages
    require(dismo)
    require(raster)
    require(rgdal)
    require(rgeos)
    
    # Create voronoi polygons
    cen <- gCentroid(unb, byid=T)
    ext <- extent(unb)
    unb.alloc <- voronoi(cen, ext = c(ext[1], ext[2], ext[3], ext[4]))
    unb.alloc <- crop(unb.alloc, fire.perim)
    unb.alloc@data$ID <- unb@data$ID
    
    # Create blank raster
    mround <- function(x,base,method){ 
      if(method == "min") {
        return(base*floor(x/base))
      } else if(method == "max"){
        return(base*ceiling(x/base))
      } else {
        stop("Unrecognized method: choose 'min' or 'max'.")
      }
    }
    
    xmin <- mround(xmin(fire.perim), 30, "min")
    xmax <- mround(xmax(fire.perim), 30, "max")
    ymin <- mround(ymin(fire.perim), 30, "min")
    ymax <- mround(ymax(fire.perim), 30, "max")
    r1 <- (xmax - xmin) / 30
    r2 <- (ymax - ymin) / 30
    blank <- raster(resolution = 30, xmn = xmin, xmx = xmax, ymn = ymin, 
                    ymx = ymax, crs = projection(fire.perim), vals = 1)
    blank <- mask(blank, fire.perim)
    
    # Rasterize UI and perimeter
    unb.r <- rasterize(unb, blank, field=2)
    fire.line <- as(fire.perim, 'SpatialLines')
    perim.r <- rasterize(fire.line, blank, field=2)
    
    # Distance to UI
    unb.r.2 <- sum(unb.r, perim.r, na.rm = T)
    unb.r.2[unb.r.2 >= 2] <- 2
    unb.r.2[unb.r.2 == 0] <- NA
    dist <- distance(unb.r.2)
    dist <- mask(dist, fire.perim)
    
    # Calculate probability of seedling presence
    seed <- (-1 / (1 + 35 * exp(-0.016 * dist))) + 1
    
    # Assign seedling presence probability to UI
    seed.sum <- extract(seed, unb.alloc, method = 'simple', small = T, 
                        fun = sum, na.rm = T, df = T, sp = T)
    return(seed.sum@data$layer)
  }
  
  # Number of federal structures
  Infrastructure <- function(unb, fire.perim){
    # Load packages
    require(rgdal)
    require(rgeos)
    require(matrixStats)
    
    # Load buildings
    #fcc <- readOGR("Infrastructure/Intermediates/FCC_Point.shp")
    #nps <- readOGR("Infrastructure/Intermediates/NPS_Point.shp")
    #usfs <- readOGR("Infrastructure/Intermediates/USFS_Point.shp")
    #blm.p <- readOGR("Infrastructure/Intermediates/BLM_Point.shp")
    #blm.l <- readOGR("Infrastructure/Intermediates/BLM_Line.shp")
    
    # Test if UI contains a building
    fcc.unb <- rowSums(gIntersects(fcc, unb, byid=T))
    nps.unb <- rowSums(gIntersects(nps, unb, byid=T))
    usfs.unb <- rowSums(gIntersects(usfs, unb, byid=T))
    blm.unb <- rowSums(gIntersects(blm.p, unb, byid=T)) + 
      rowSums(gIntersects(blm.l, unb, byid=T))
    infra.unb <- fcc.unb + nps.unb + usfs.unb + blm.unb
    
    return(infra.unb)
  }
  
  # Define function to calculate mean stand age
  StandAge <- function(unb, fire.perim){
    # Load packages
    require(rgdal)
    require(raster)
    
    # Load data
    age <- raster("StandAge/StandAge.clip.tif")
    
    # Transform projection
    fire.proj <- spTransform(fire.perim, CRS(projection(age)))
    unb.proj <- spTransform(unb, CRS(projection(age)))
    
    # Clip to fire extent
    age <- crop(age, extent(fire.proj))
    
    # Mask stand age to UIs
    age.unb <- mask(age, unb.proj)
    
    # Extract mean stand age values for each UI
    unb.age <- extract(age.unb, unb.proj, small=T, fun=mean, na.rm=T)
    unb.age[is.na(unb.age)] <- 0
    
    return(as.numeric(unb.age))
  }
  
  # Critical habitat function (1.04 min)
  CritHabitat <- function(unb){
    # Load packages
    require(rgdal)
    require(raster)
    require(rgeos)
    
    # Load Critical Habitat data
    #hab <- readOGR("CriticalHabitat/CriticalHabitat.shp")
    
    # Identify UIs with Critical Habitat
    int <- suppressWarnings({intersect(hab, unb)})
    if(is.null(int)){
      int <- data.frame(ID = NA, listing_st = NA)
    }
    
    # Add score based upon type of species
    h.score <- data.frame(listing_st= c("Endangered", "Threatened", 
                                        "Recovery", "Proposed Endangered"),
                          score.habitat= c(1,.5,.2,.2))
    hab.unb <- merge(int[, c("ID", "listing_st")], h.score, by= "listing_st")
    hab.unb <- merge(unb@data, hab.unb, by="ID", all=T)$score.habitat
    hab.unb[is.na(hab.unb)] <- 0
    return(as.numeric(hab.unb))
  }
  
  # Define proportion of invasive species cover
  Invasive <- function(unb, fire.perim){
    # Load packages
    require(rgdal)
    require(rgeos)
    require(raster)
    
    # Load invasive species data
    #inv <- readOGR("Invasive/Invasive.shp")
    
    # Clip data to fire perimeter
    inv <- crop(inv, fire.perim)
    if(is.null(inv)){
      out <- rep(0, nrow(unb))
    } else {
      inv <- gIntersection(unb, inv, byid = T)
      if(is.null(inv)){
        out <- rep(0, nrow(unb))
      } else {
        unb.over <- over(inv, unb, returnList = F)
        inv.area <- sapply(slot(inv, "polygons"), function(i) slot(i, "area"))
        inv.df <- data.frame(ID = unb.over$ID,
                             inv.area = inv.area)
        inv.df <- aggregate(inv.area ~ ID, data = inv.df, FUN = sum)
        inv.df$unb.area <- sapply(slot(unb[unb@data$ID %in% inv.df$ID,], "polygons"), 
                                  function(i) slot(i, "area"))
        inv.df$prop.inv <- inv.df$inv.area / inv.df$unb.area
        inv.df <- merge(data.frame(ID = unb@data$ID), inv.df[, c(1,4)], all = T)
        inv.df$prop.inv[is.na(inv.df$prop.inv)] <- 0
        out <- inv.df$prop.inv
      }
      
      
    }
    return(out)
  }
  
  Col <- function(unb, crit){
    cl <- data.frame(crit = sort(unique(crit)), 
                     col = colorRampPalette(c("green", "yellow", "orange", "red"))
                     (length(unique(crit))))
    cl <- merge(data.frame(ID = unb@data$ID, crit = crit), cl, by = "crit")
    cl <- as.character(cl[order(cl$ID), "col"])
  }
}

shinyApp(ui, server)



#############################
library(reticulate)
system('C:\\Python27\\ArcGIS10.4\\python.exe import sys')
system('S:\\COS\\PyroGeog\\amartinez\\Ranking\\ShinyData\\python.exe version.py')
a <- py_run_file('S:\\COS\\PyroGeog\\amartinez\\Ranking\\ShinyData\\add.py', convert = T)
py_run_string("import sys")
py_run_string({"print (sys.version)"})
import("sys")
use_python("C:\\Python27\\x64\\python.exe")
initialize_python()


###
source_python('S:\\COS\\PyroGeog\\amartinez\\Ranking\\ShinyData\\add.py')
add(5,10)
py_run_string('x = 10')
py$x
