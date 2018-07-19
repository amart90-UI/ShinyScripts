library(shiny)

#############################################################

ui <- fluidPage(
  p("Select a fire below.  You may filter the list by the firest sart location, year or MTBS fire ID"),
  checkboxGroupInput(inputId = "inStateGroup", label = "Input state",
                     choices = c("WA", "OR", "ID", "Other"), selected = c("WA", "OR", "ID", "Other")),
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
  plotOutput(outputId = "fireplotzoom"),
  textOutput(outputId = "fireID")
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
  output$fireplotzoom <- renderPlot({plot(fire.perim[fire.perim@data$FireDesc %in% fire(),])})
  output$fireID <- renderText({unique(fire.perim$Fire_ID[fire.perim@data$FireDesc %in% fire()])})
  
  
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
