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


ui <- fluidPage( #theme = shinytheme("superhero"),
  
  
  titlePanel("Stock Market"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput("Company",
                     NULL,
                     choices= c('Enter company name' = '', unique(names(myelements))),
                     selected= NULL,
                     multiple=FALSE,
                     options = NULL),
      
      dateRangeInput(inputId = 'Date', 
                     h3("Date range"),
                     start = Sys.Date() - 30,
                     end   = Sys.Date() - 1,
                     min    = Sys.Date() - 730,
                     max    = Sys.Date(),
                     format = "yy/mm/dd",
                     separator = " - ")
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "distPlot")), 
        tabPanel("Interactive Plot", dygraphOutput("dygraph"))
      )
      
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    req(input$Company)
    # myTimestamp <- input$Date
    start <- format(input$Date[1])
    end <- format(input$Date[2])
    data <- ds.getSymbol.yahoo(myelements[input$Company], from = start, to = end)
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
    start <- input$Date[1]
    end <- input$Date[2]
    data <- ds.getSymbol.yahoo(myelements[input$Company], from = start, to = end)
    dataAux <- data[,c(1, 4)]
    names(dataAux) <- c("Open", "Close")
    # ggplotly(myplot)
    dygraph(dataAux) %>% dyRangeSelector()
    
  })
  
  
}

shinyApp(ui = ui, server = server)
