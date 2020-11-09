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

elements <- read.csv("./data/stock_market.csv", header = TRUE, sep = ';', fileEncoding = "UTF-8")
myelements <- unname(unlist(elements['Ticker']))
names(myelements) <- unname(unlist(elements['Name']))

# Symbol <- "SAN.MC"

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("superhero"),
  
  # App title ----
  titlePanel("Stock Market"),
  
  sidebarLayout(
    # Main panel for displaying outputs ----
    sidebarPanel(
      
      selectizeInput("Company",
                     NULL,
                     # choices= c('Enter company name' = '', unique(companies)),
                     choices= c('Enter company name' = '', unique(names(myelements))),
                     # multiple=TRUE,
                     selected= NULL,
                     multiple=FALSE,
                     options = NULL),
      
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
    req(input$Company)
    myTimestamp <- input$Date
    data <- ds.getSymbol.yahoo(myelements[input$Company], from = Sys.Date() - myTimestamp, to = Sys.Date())
    dataAux <- data[,c(1, 4)]
    dataAux <- data.frame(date=index(dataAux), coredata(dataAux))
    dataAux <- melt(data = dataAux, id.vars = c("date"), measure.vars = c(2,3))
    
    ggplot(dataAux, aes(x = date, y = value, colour = variable)) + 
      geom_line() + 
      ggtitle(input$Company) + 
      theme(plot.title = element_text(size=14, face="bold"))
      
    
  })
  
}

shinyApp(ui = ui, server = server)
