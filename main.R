
# Install and load packages
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

# install.packages("tseries")
# library(tseries)

# Load file with company names and Tickers to extract values from yahoo finances
elements <- read.csv("./data/stock_market.csv", header = TRUE, sep = ';', fileEncoding = "UTF-8")

# Create variable with company names and their Symbols as values
CompanySymbol <- unname(unlist(elements['Ticker']))
names(CompanySymbol) <- unname(unlist(elements['Name']))

# Variable with the type of values to display
vNames <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
names(vNames) <- 1:6

# Symbol <- "SAN.MC"


# Defining User Interface
ui <- fluidPage( 
  # apply 'sandstone' theme
  theme = shinytheme("sandstone"),
  
  # Add title panel
  titlePanel("Stock Market Screener"),
  
  
  sidebarLayout(
    
    # Create sidebar panel
    sidebarPanel(
      
      # Create section to select the company
      selectizeInput("Company",
                     h3("Company name"),
                     choices= c('Enter company name' = '', unique(names(CompanySymbol))),
                     selected= NULL,
                     multiple=FALSE,
                     options = NULL),
      
      # Create section to select the dates of the date range
      radioButtons("ranges", 
                   h3("Date range"),
                   choices = list("1 week" = 1, 
                                  "1 year" = 2, 
                                  "5 years" = 3, 
                                  "Customized" = 4),selected = 4),
      dateRangeInput(inputId = 'Date',  label = NULL,
                     start = Sys.Date() - 90,
                     end   = Sys.Date() - 1,
                     max    = Sys.Date(),
                     format = "dd/mm/yyyy",
                     separator = " - "),
      
      # Create section to select the values to display in plot
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
    
    # Main Panel
    mainPanel(
      
      # Create navbar to select between the standard plot and the interactive plot
      navbarPage(NULL,
                 tabPanel("Plot",
                          # Standard plot
                          plotOutput(outputId = "distPlot")
                          
                 ),
                 tabPanel("Table",
                          # Table with relevant info
                          tableOutput("InfoValues")
                 ),
                 tabPanel("Interactive",
                          # Interactive plot
                          dygraphOutput("dygraph")
                 )
                 
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  # Implement observations to automate the updates of the inputs in the date range
  observeEvent(input$ranges, {
    aux <<- input$ranges
    if (input$ranges == "1"){
      updateDateRangeInput(session,
                           'Date',
                           start = Sys.Date() - 7)
    }
    else if (input$ranges == "2"){
      updateDateRangeInput(session,
                           'Date',
                           start = Sys.Date() - 365)
    }
    else if (input$ranges == "3"){
      updateDateRangeInput(session,
                           'Date',
                           start = Sys.Date() - 1825)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$Date, {
    myDates <<- input$Date
    if (input$Date[1]== Sys.Date() - 7){
      updateRadioButtons(session,
                         'ranges',
                         selected = 1)
    }
    else if (input$Date[1]== Sys.Date() - 365){
      updateRadioButtons(session,
                         'ranges',
                         selected = 2)
    }
    else if (input$Date[1]== Sys.Date() - 1825){
      updateRadioButtons(session,
                         'ranges',
                         selected = 3)
    }
    else {
      updateRadioButtons(session,
                         'ranges',
                         selected = 4)
    }
  }, ignoreInit = TRUE)
  
  dataInput <- reactive({
    # Added 'require' to wait until the user selects a company
    req(input$Company)
    
    # Give values from the date range selected
    start <- format(input$Date[1])
    end <- format(input$Date[2])
    
    # Use 'rtsdata' to obtain data from yahoo finances using the Symbol from the company selected in the interface
    data <- ds.getSymbol.yahoo(CompanySymbol[input$Company], from = start, to = end)
    
    # Transform to integer the variables selected from the check box to filter dataframe for those values
    variablesSelected <<- as.integer(input$variableSelect)
    dataAux <- data[,variablesSelected]
    names(dataAux) <- vNames[variablesSelected]
    return(dataAux)
  })
  # Regular plot
  output$distPlot <- renderPlot({
    
    # Create dataframe from the xts object
    dataAux <- data.frame(date=index(dataInput()), coredata(dataInput()))
    
    # Use function melt to define proper structure of Dataframe to represent with ggplot
    dataAux <- melt(data = dataAux, id.vars = c("date"), measure.vars = c(2:ncol(dataAux)))
    
    # Display dataframe
    ggplot(dataAux, aes(x = date, y = value, colour = variable)) + 
      geom_line() + 
      geom_point() +
      ggtitle(input$Company) +
      theme(plot.title = element_text(size=14, face="bold")) +
      labs (colour = "Values")
    
  })
  
  # Interactive plot
  output$dygraph <- renderDygraph({
    
    # Display interactive plot
    dygraph(dataInput()) %>% dyRangeSelector() #%>% 
    # dyShading(from = start, to = end, color = "white")
    
  })
  
  
  # Table  ----
  output$InfoValues <- renderTable({
    
    # Transform to integer the variables selected from the check box to filter dataframe for those values
    variablesSelected <- as.integer(input$variableSelect)
    
    # create empty dataframe for our table
    tableInfo <- data.frame(Min=numeric(),
                            Max=numeric(),
                            Avg=numeric(),
                            Median=numeric(),
                            Standard_deviation=numeric(),
                            Sd=character(),
                            stringsAsFactors=FALSE)
    
    names(tableInfo)[names(tableInfo) == "Sd"] <- "Sd(%)"
    
    # Fill the table
    for (i in 1:length(variablesSelected)){
      tableInfo[nrow(tableInfo)+1,] <- c("Min" = min(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE), 
                                         "Max" = max(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE),
                                         "Avg" = round(mean(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE), 2),
                                         "Median" = round(median(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE),  2),
                                         "Standard deviation" = round(sd(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE),  2),
                                         "Sd(%)"= paste(round(sd(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE) / mean(as.numeric(coredata(dataInput()[,i])), na.rm = TRUE), 3)*100, "%"))
      
    }
    rownames(tableInfo) <- vNames[variablesSelected]
    tableInfo
  }, rownames = TRUE)
  
}

# Start application
shinyApp(ui = ui, server = server)

