library(shiny)
firelist <- data2

#############################################################

ui <- fluidPage(
  p("The checkbox group controls the select input"),
  checkboxGroupInput(inputId = "inStateGroup", label = "Input state",
                     choices = c("WA", "OR", "ID", "Other"), selected = c("WA", "OR", "ID", "Other")),
  sliderInput(inputId = "inYearSlide", label = "Select year range", min = 1984, max = 2014, value = c(1984, 2014)),
  selectInput(inputId = "inSelect", label = "Select input", choices = firelist$FireDesc),
  fluidRow(
    column(12,
           tableOutput('table')
    )
  ),
  plotOutput(outputId = "fireplot"),
  actionButton(inputId = "do", label = "Click Me"),
  plotOutput(outputId = "fireplotzoom")
)

server <- function(input, output, session) {
  observe({
    x <- input$inStateGroup
    if("Other" %in% x){
      x <- c(x[-which(x == "Other")], "CA", "MT", "NV", "UT", "WY")
    }
    y <- input$inYearSlide[1]:input$inYearSlide[2]
    FireChoices <- firelist$FireDesc[substr(firelist$Fire_ID, 1, 2) %in% x & firelist$Year %in% y]
    # Can also set the label and select items
    updateSelectInput(session, "inSelect",
                      label = paste("Select input label (", length(FireChoices), ")"),
                      choices = FireChoices,
                      selected = head(x, 1)
    )
  })
  output$fireplot <- renderPlot({
    plot(pnw.map)
    plot(fire.perim[fire.perim@data$FireDesc == input$inSelect,], col = "red", border = "red", lwd = 3, add = T)
  })
  output$table <- renderTable(firelist[firelist$FireDesc == input$inSelect,c("Fire_Name", "Year", "Acres", "StartDate", "Fire_ID")])
  fire <- eventReactive(input$do, {input$inSelect})
  output$fireplotzoom <- renderPlot({plot(fire.perim[fire.perim@data$FireDesc %in% fire(),])})

}

shinyApp(ui, server)

#############################


