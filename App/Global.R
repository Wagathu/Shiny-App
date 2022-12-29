
library(readxl) 
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(stringi)
library(lubridate)
library(shinyWidgets)
library(slickR)
library(htmlwidgets)

counties <- read_excel("Data/POPULATIONCOUNTIES.xlsx")

population <- read_excel("Data/PopGrowthRate.xlsx")

population <- read_excel("Data/PopGrowthRate.xlsx")

Fertility <- read_excel("Data/Fertility Rate.xlsx")

Mortality <- read_excel("Data/Mortality Rate.xlsx")

Expectancy <- read_excel("Data/Life Expectancy.xlsx")

Gdp <- read_excel("Data/Gross Domestic Product.xlsx")

Gdp <- read_excel("Data/Gross Domestic Product.xlsx")

infantM <- read_excel("Data/Infant Mortality Rate.xlsx")

Unemployment <- read_excel("Data/Unemployment Rate.xlsx")

Education <- read_excel("Data/Education Spending.xlsx")

Co2 <- read_excel("Data/Co2.xlsx")

Electricity <- read_excel("Data/Electricity Access.xlsx")

imgs = c("kenya.png","kenya2.png","kenya3.png","kenya4.png","kenya5.png","kenya6.png","kenya7.png","wild.png")

 