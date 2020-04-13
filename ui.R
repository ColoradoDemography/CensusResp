# SYA by Race Chart Dashboard
# Adam Bickford January 2020
# These are the UI Object and the refers to the web objects in the index.html file...

library(tidyverse)
library(shiny)

source("setup.R")
  CENSUS_KEY="08fe07c2a7bf781b7771d7cccb264fe7ff8965ce"

  datelist  <- Date_Check(CENSUS_KEY)



function(req) {
  htmlTemplate("index.html",
                datesel=selectInput("datesel","Select a date:", choices= datelist),  # Build this from data set
                goBtn = actionButton("goButton","Generate Map"),
                tractMap = leafletOutput("outmap"),
                dlBtn = downloadButton("CHDATA","Download Report"))
 }



 
