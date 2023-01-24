

####DEFINE THE PROJECT FOLDER BEFORE EXECUTING ANOTHER OPERATION #####
###THIS LINE HAS TO BE CHANGED DEPENDING ON THE COMPUTER THE PROJECT IS CURRENTLY WORKING###
setwd("E:/Romuald/SIA/Analyse de donnees/Projet/Template/Projet2021_AnalyseDeDonnees")


# load required packages
install.packages("magrittr")
install.packages("rvest")
install.packages("readxl")
install.packages("maps")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("ggiraph")
install.packages("RColorBrewer")
install.packages("leaflet")
install.packages("plotly")
# install.packages("geojsonio")
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("shinydashboard")
install.packages("shinythemes")
install.packages("ellipsis")
install.packages("dplyr")
# install.packages("stringr")
install.packages("RcppRoll")
install.packages("plotly")
install.packages('rsconnect')
install.packages('readr')


library(magrittr)
library(rvest)
library(readxl)
library(maps)
require(ggplot2)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(leaflet)
library(geojsonio)
require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinythemes)
require(dplyr)
require(ellipsis)
library(stringr)
require(readr)
require(rsconnect)
require(RcppRoll)






#calling the processing functions

source("processing.r")
  #calling the ui and the server
source("ui.r")
source("server.r")

# Running a Shiny app object

df=read.csv("FORCE-G_LINEAR_ACCELERATOR_GYROSCOPE_INPUT_N.csv",sep=";")
df=clean_dataframe(df)
df=extract_feature_large(df,0.1,0,0.5)


app <- shinyApp(ui,server)
runApp(app)


