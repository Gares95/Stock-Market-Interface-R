# install.packages("shiny")
# install.packages("ggplot2")
library(shiny)
library(ggplot2)
# install.packages("rtsdata")
library(rtsdata)
# install.packages("reshape")
library(reshape)

# install.packages("shinythemes")
library(shinythemes)

# install.packages("plotly")
# library(plotly)
####################################
# install.packages("dygraphs")
library(dygraphs)
###################################

elements <- read.csv("./data/stock_market.csv", header = TRUE, sep = ';', fileEncoding = "UTF-8")
myelements <- unname(unlist(elements['Ticker']))
names(myelements) <- unname(unlist(elements['Name']))

# Symbol <- "SAN.MC"

# Define UI for app that draws a histogram ----
ui <- fluidPage( #theme = shinytheme("superhero"),
  
  # App title ----
  titlePanel("Stock Market"),
  
  sidebarLayout(
    # Main panel for displaying outputs ----
    sidebarPanel(
      
      selectizeInput("Company",
                     NULL,
                     choices= c('Enter company name' = '', unique(names(myelements))),
                     selected= NULL,
                     multiple=FALSE,
                     options = NULL),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Date",
                  label = "Days back:",
                  min = 1,
                  max = 365,
                  value = 120)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "distPlot")), 
        tabPanel("Interactive Plot", dygraphOutput("dygraph"))
      )
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    req(input$Company)
    myTimestamp <- input$Date
    data <- ds.getSymbol.yahoo(myelements[input$Company], from = Sys.Date() - myTimestamp, to = Sys.Date())
    dataAux <- data[,c(1, 4)]
    names(dataAux) <- c("Open", "Close")
    
    dataAux <- data.frame(date=index(dataAux), coredata(dataAux))
    dataAux <- melt(data = dataAux, id.vars = c("date"), measure.vars = c(2,3))
    
    myplot <- ggplot(dataAux, aes(x = date, y = value, colour = variable)) + 
      geom_line() + 
      geom_point() +
      ggtitle(input$Company) +
      theme(plot.title = element_text(size=14, face="bold")) +
      labs (colour = "Open and Close Values")
    
    myplot
      
  })
  
  output$dygraph <- renderDygraph({
    req(input$Company)
    myTimestamp <- input$Date
    data <- ds.getSymbol.yahoo(myelements[input$Company], from = Sys.Date() - myTimestamp, to = Sys.Date())
    dataAux <- data[,c(1, 4)]
    names(dataAux) <- c("Open", "Close")
    # ggplotly(myplot)
    dygraph(dataAux) %>% dyRangeSelector()
    
  })
  
  
}

shinyApp(ui = ui, server = server)
