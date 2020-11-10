# Data Lake with Spark
***
This repository includes the files neccessary to build an interface using the **R** package _Shiny_ to display information of the stock market. It includes tools to select the company (from the dataset uploaded with over 1890 companies in 50 different countries around the world) and the time period to display. 

The first version of this repository allows to see the _Open_ and _Close_ values of the stock market for each company during this year.

The interface includes an interactive plot.

# Data Files
***
The dataset used for this project is:
- stock_market.csv: This file contains information of each company, including:
    - Name
    - Sector
    - Ticker
    - Country
    
# Requirements
For the interface to work it is necessary to install the next packages:  
- shiny  
- ggplot2  
- rtsdata  
- reshape  
- shinythemes  
- dygraphs  

# Images of the interface

![alt text](https://raw.githubusercontent.com/Gares95/Stock-Market-Interface-R/master/Img/Pasive.PNG)

![alt text](https://raw.githubusercontent.com/Gares95/Stock-Market-Interface-R/master/Img/MainPage.PNG)

![alt text](https://raw.githubusercontent.com/Gares95/Stock-Market-Interface-R/master/Img/Interactive2.png)