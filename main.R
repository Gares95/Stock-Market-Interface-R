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

vNames <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
names(vNames) <- 1:6
# Symbol <- "SAN.MC"

ui <- fluidPage( theme = shinytheme("sandstone"),
  
  titlePanel("Stock Market Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectizeInput("Company",
                     h3("Company name"),
                     choices= c('Enter company name' = '', unique(names(myelements))),
                     selected= NULL,
                     multiple=FALSE,
                     options = NULL),
      
      dateRangeInput(inputId = 'Date', 
                     h3("Date range"),
                     start = Sys.Date() - 90,
                     end   = Sys.Date() - 1,
                     min    = Sys.Date() - 730,
                     max    = Sys.Date(),
                     format = "dd/mm/yyyy",
                     separator = " - "),
      
      checkboxGroupInput("variableSelect", 
                         h3("Values"), 
                         choices = list("Open Values" = 1, 
                                        "Close Values" = 4, 
                                        "High" = 2,
                                        "Low" = 3, 
                                        "Volume" = 5,
                                        "Adjusted" = 6),
                         selected = 1)
    ),
    
    mainPanel(
      navbarPage(NULL,
                 tabPanel("Plot",
                          plotOutput(outputId = "distPlot")
                          
                 ),
                 tabPanel("Interactive",
                          dygraphOutput("dygraph")
                 )
                 
      )
      
      
      
      # tabsetPanel(
      #   tabPanel("Plot", plotOutput(outputId = "distPlot")), 
      #   tabPanel("Interactive Plot", dygraphOutput("dygraph"))
      # )
      
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    req(input$Company)
    
    start <- format(input$Date[1])
    end <- format(input$Date[2])
    data <- ds.getSymbol.yahoo(myelements[input$Company], from = start, to = end)
    variablesSelected <<- as.integer(input$variableSelect)
    dataAux <- data[,variablesSelected]
    names(dataAux) <- vNames[variablesSelected]
    
    dataAux <- data.frame(date=index(dataAux), coredata(dataAux))
    dataAux <- melt(data = dataAux, id.vars = c("date"), measure.vars = c(2:ncol(dataAux)))
    
    ggplot(dataAux, aes(x = date, y = value, colour = variable)) + 
      geom_line() + 
      geom_point() +
      ggtitle(input$Company) +
      theme(plot.title = element_text(size=14, face="bold")) +
      labs (colour = "Values")
    
  })
  
  output$dygraph <- renderDygraph({
    req(input$Company)
    start <- input$Date[1]
    end <- input$Date[2]
    data <- ds.getSymbol.yahoo(myelements[input$Company], from = start, to = end)
    # dataAux <- data[,c(1, 4)]
    # names(dataAux) <- c("Open", "Close")
    variablesSelected <<- as.integer(input$variableSelect)
    dataAux <- data[,variablesSelected]
    names(dataAux) <- vNames[variablesSelected]
    # ggplotly(myplot)
    dygraph(dataAux) %>% dyRangeSelector() #%>% 
      # dyShading(from = start, to = end, color = "white")
    
  })
  
}

shinyApp(ui = ui, server = server)
