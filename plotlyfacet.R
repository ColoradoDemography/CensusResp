# SYA by Race Chart Dashboard
# Adam Bickford January 2020
# 

rm(list = ls())
setwd("J:/Community Profiles/Shiny Demos/age by Race")
library(tidyverse)
library(RPostgreSQL)
library(readr)
library(readxl)
library(gdata)
library(scales)
library(RColorBrewer)
library(plotly)


# Additions for Database pool
library('pool') 
library('DBI')
library('stringr')
library('config')

# Set up database pool 1/23/19

config <- get("database")
DOLAPool <-  dbPool(
  drv <- dbDriver(config$Driver),
  dbname = config$Database,
  host = config$Server,
  port = config$Port,
  user = config$UID,
  password = config$PWD
)



# Support Functions
# NumFmt formats a numberic variable to a whold number, comma separated value
#
NumFmt <- function(inval){
  outval <- format(round(inval ,digits=0),  big.mark=",")
  return(outval)
}



#genMax Returns the maximum value for a given data set, for the y-axis, for a selected county and year
genMax <- function(inData) {

   f.indataH <- inData %>% 
             filter(ethnicity == "Hispanic Origin") %>%
             group_by(age) %>%
             summarise(pop = sum(count)) %>%
             mutate(race = "Hispanic Origin")
   f.indataOth <- inData %>% 
             filter(ethnicity != "Hispanic Origin") %>%
             group_by(race,age) %>%
             summarise(pop = sum(count)) 
   f.indatFin <- bind_rows(f.indataH, f.indataOth)
  
   maxPop <- max(f.indatFin$pop)
   return(maxPop)
   
}



#SYA by Race  setting defaults


#Generates dataeset for plotting

genPlotData <- function(DBPool,fips,yr){
  if(fips == 0) {
       sqlSYARace <- paste0("SELECT * FROM estimates.county_sya_race_estimates WHERE year = ",yr,";")
  } else {
       sqlSYARace <- paste0("SELECT * FROM estimates.county_sya_race_estimates WHERE (county_fips = ",fips,"AND year = ",yr,");")
  }

 f.SYARace <-  dbGetQuery(DBPool, sqlSYARace) 
 # Calculating the overall maximum
 MaxVal <- genMax(f.SYARace)

   f.SYARaceHisp <- f.SYARace %>% 
             filter(ethnicity == "Hispanic Origin") %>%
             mutate(race = "Hispanic Origin") %>%
             group_by(race,age) %>%
             summarise(Population = sum(count)) 
   
   f.SYARaceNHisp <- f.SYARace %>% 
             filter(ethnicity != "Hispanic Origin") %>%
             group_by(race,age) %>%
             summarise(Population = sum(count))
 
    f.SYARaceOut <- bind_rows(f.SYARaceHisp, f.SYARaceNHisp)
    f.SYARaceOut$Population <- ceiling(f.SYARaceOut$Population)
    names(f.SYARaceOut)[2] <- "Age"

outList <- list("data" = f.SYARaceOut,"maxval" = MaxVal) 
return(outList)
}



# Adams County
ctyfips <- 13
year <- 2018

plt_data <- genPlotData(DBPool = DOLAPool,fips = ctyfips,yr = year)
# Generate the plotly pairs
f.SYARace <- plt_data$data

f.SYARace$race <- plyr::revalue(f.SYARace$race, c("Hispanic Origin" = "Hispanic",
                                                  "American Indian" = "American Indian, Not Hispanic",
                                                  "Asian/Pacific Islander" = "Asian/Pacific Islander, Not-Hispanic",
                                                  "Black" = "Black, Not Hispanic",
                                                  "White" = "White, Not Hispanic"))

outCAP <- paste0("Colorado State Demography Office, Date Printed: ",as.character(format(Sys.Date(),"%m/%d/%Y")))
grTitle <- paste0("Single Year of Age by Race, ",year)  # Add County to this
maxVAL <- plyr::round_any(plt_data$maxval,100,ceiling) 


 maxIND <- f.SYARace %>%
    group_by(race) %>%
    summarize(maxPop = max(Population))
   
   maxIND$maxPop <- plyr::round_any(maxIND$maxPop,100,ceiling) 
   
   
   f.SYARace <- inner_join(f.SYARace,maxIND,by="race")

   f.SYARace[is.na(f.SYARace)] <- 0

 ggSYASTD <- f.SYARace %>% 
  group_by(race) %>%
  do(plot = ggplot(data= .) + aes(x=Age, y=Population) +
  geom_bar(stat="identity", position="dodge", color="black", fill="deepskyblue")+
  scale_y_continuous(limits = c(0, maxVAL), label=comma) +
  scale_x_continuous(limits=c(0,85),breaks=seq(0,85,5)) +
  labs(title = grTitle,
       caption = outCAP,
       x = "Age",
       y= "Population") +
  theme(plot.title = element_text(hjust = 0.5, size=12),
        plot.caption = element_text(hjust = 0, size=9),
        panel.background = element_rect(fill = "white", colour = "gray50"),
        panel.grid.major = element_line(colour = "gray80"),
        axis.text.x = element_text(size=10),
        axis.text.y=element_text(size=10)))




ggSYAIND <-  f.SYARace  %>%
             group_by(race) %>%
              do(plot= ggplot(data=.) + aes(x=Age, y=Population) +
  geom_bar(stat="identity", position="dodge", color="black", fill="lightblue")+
  scale_y_continuous(limits = c(0, unique(.$maxPop)), label=comma) +
  scale_x_continuous(limits=c(0,85),breaks=seq(0,85,5)) +
  labs(title = grTitle,
       caption = outCAP,
       x = "Age",
       y= "Population") +
  theme(plot.title = element_text(hjust = 0.5, size=12),
        plot.caption = element_text(hjust = 0, size=9),
        panel.background = element_rect(fill = "white", colour = "gray50"),
        panel.grid.major = element_line(colour = "gray80"),
        axis.text.x = element_text(size=10),
        axis.text.y=element_text(size=10)))

STDOut <- list()
INDOut <- list()

for(i in 1:5) {
   name = paste("plot", i, sep = "_")
  STDOut[[name]] <- plotly_build(ggplotly(ggSYASTD[[i,2]]))
   
  STDOut[[name]] <- STDOut[[name]] %>% layout(title = list(text = paste0(grTitle,
                                    '<br>',
                                    '<sup>',
                                    ggSYASTD[[i,1]],
                                    '</sup>')),
                  annotations= list(text=outCAP, font = list(size = 10), showarrow=F, yref='paper', y=0)) 
  
  INDOut[[name]] <- plotly_build(ggplotly(ggSYAIND[[i,2]]))
   
  INDOut[[name]] <- INDOut[[name]] %>% layout(title = list(text = paste0(grTitle,
                                    '<br>',
                                    '<sup>',
                                    ggSYAIND[[i,1]],
                                    '</sup>')),
                  annotations= list(text=outCAP, font = list(size = 10), showarrow=F, yref='paper', y=0))   
}







