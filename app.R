library(shiny)

#############################################################

ui <- fluidPage(
  p("Select a fire below.  You may filter the list by the firest sart location, year or MTBS fire ID"),
  checkboxGroupInput(inputId = "inStateGroup", label = "Input state",
                     choices = c("WA", "OR", "ID", "Other"), 
                     selected = c("WA", "OR", "ID", "Other")),
  sliderInput(inputId = "inYearSlide", label = "Select year range", min = 1984, max = 2014, value = c(1984, 2014), sep = ""),
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
              choices = c("Size", "Isolation")),
  plotOutput(outputId = "fireplotzoom"),
  downloadButton(outputId = "downloadFire", label = 'Download fire perimeter')
)

server <- function(input, output, session) {
  
  observe({
    x <- input$inStateGroup
    if("Other" %in% x){
      x <- c(x[-which(x == "Other")], "CA", "MT", "NV", "UT", "WY")
    }
    y <- input$inYearSlide[1]:input$inYearSlide[2]
    z <- ifelse(is.null(input$inIdText),firelist$Fire_ID, input$inIdText)
    FireChoices <- firelist$FireDesc[substr(firelist$Fire_ID, 1, 2) %in% x 
                                     & firelist$Year %in% y 
                                     & firelist$Fire_ID %in% firelist$Fire_ID[grep(z, firelist$Fire_ID)]]
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
  #eventReactive(input$do, {assign(unb.sel()@data$size, Size(unb.sel))})
  #size <- eventReactive(input$do,{
  #  if("Size" %in% input$inCrit){Size(unb.sel())
  #    }})
  #isol <- eventReactive(input$do, {
  #  if("Isloation" %in% input$inCrit){Isolation(unb.sel(), fire.sel())
  #    }})
  size <- eventReactive(input$do, {Size(unb.sel())})
  isol <- eventReactive(input$do, {Isolation(unb.sel(), fire.sel())})
  col <- eventReactive(input$do, {
    if(input$inCrit == "Size"){
      Col(unb.sel(), size())
    } else if(input$inCrit == "Isolation"){
      Col(unb.sel(), isol())
    }
  })
  
  output$fireplotzoom <- renderPlot({
    plot(fire.sel(),)
    plot(unb.sel(), add = T, col = col(), border = col())
  })
  
  output$downloadFire <- downloadHandler(
    filename = 'fire_perim.zip',
    content = function(file) {
      if (length(Sys.glob("fire_perim.*"))>0){
        file.remove(Sys.glob("fire_perim.*"))
      }
      writeOGR(fire.sel(), dsn="fire_perim.shp", layer="fire_perim", driver="ESRI Shapefile")
      #write.csv(as.data.frame(cbind(getGeoContent()@data, as.data.frame(getGeoContent()@coords))), "fbCrawl.csv")
      zip(zipfile='fire_perim.zip', files=Sys.glob("fire_perim.*"))
      file.copy("fire_perim.zip", file)
      if (length(Sys.glob("fire_perim.*"))>0){
        file.remove(Sys.glob("fire_perim.*"))
      }
    }
  )
  
  # Define Functions
  Size <- function(ui){
    require(rgeos)
    # Calculate size of unburned islands
    size.ui <- gArea(ui, byid = T)
    return(size.ui)
  }
  
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
  
  Col <- function(unb.in, crit){
    cl <- data.frame(crit = sort(unique(crit)), 
                     col = colorRampPalette(c("green", "yellow", "orange", "red"))
                     (length(unique(crit))))
    cl <- merge(data.frame(ID = unb.in@data$ID, crit = crit), cl, by = "crit")
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
