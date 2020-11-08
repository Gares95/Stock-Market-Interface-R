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
    dataAux <- data[,c("SAN.MC.Open", "SAN.MC.Close")]
    dataAux <- data.frame(date=index(dataAux), coredata(dataAux))
    dataAux <- melt(data = dataAux, id.vars = c("date"), measure.vars = c("SAN.MC.Open", "SAN.MC.Close"))
    
    # ggplot(data = dataAux, aes(x=Index)) + 
    #   geom_line(aes(y = SAN.MC.Open), colour = "blue") + 
    #   geom_line(aes(y = SAN.MC.Close), colour = "red") +
    #   # theme(plot.background = element_rect(fill = "#1a080a"), axis.text = element_text(colour = "white"), axis.title = element_text(colour = "white"), legend.position="top")
    #   theme(legend.position="top")
    ggplot(dataAux, aes(x = date, y = value, colour = variable)) + 
      geom_line()
    
  })
  
}

shinyApp(ui = ui, server = server)
