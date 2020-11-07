# install.packages("shiny")
# install.packages("ggplot2")
library(shiny)
library(ggplot2)
# install.packages("rtsdata")
library(rtsdata)


Symbol <- "SAN.MC"

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Stock Market - Santander Bank"),
  
  sidebarLayout(
    # Main panel for displaying outputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Date",
                  label = "Days back:",
                  min = 1,
                  max = 365,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    myTimestamp <- input$Date
    data <- ds.getSymbol.yahoo(Symbol, from = Sys.Date() - myTimestamp, to = Sys.Date())
    dataAux <- data[,"SAN.MC.Open"]
    ggplot(dataAux, aes(x = Index, y = SAN.MC.Open)) + 
      geom_line(colour = "blue") + 
      theme(plot.background = element_rect(fill = "#1a080a"), axis.text = element_text(colour = "white"), axis.title = element_text(colour = "white"))
    
  })
  
}

shinyApp(ui = ui, server = server)
