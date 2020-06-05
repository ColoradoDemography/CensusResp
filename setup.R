# Census Response Rate Map Support functions
# Adam Bickford April 2020
# This is where the program runs.

#The underlying data are based in csv files and written to CSV files. 
#Date_Check compares the latest date from the api to data in the National dataset
# Write_data function reads records from the api and updates the data files
# Map_output produces the map.
#Report_Gen outputs a report for a selected date (for download)

library(tidyverse)
library(censusapi)
library(scales)
library(officer)
library(flextable)
library(leaflet)
library(sf)
library(tigris)
library(htmltools)




# Support Functions
#Returns values as percentage
percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC( x, format = format, digits = digits, ...), "%")
}


#Creates row percentage table.
rowTab <- function(quart,crossVar,tabType) {
  #Generates a frequency table with row percentages, returns flextable 
  
  tab1 <- addmargins(table(quart,crossVar),1)
  tab2 <- prop.table(tab1,1)
  tab22 <- addmargins(tab2)
  tab23 <- as.data.frame(tab22[1:5,]) 
  tab23 <- tab23 %>% spread(crossVar,Freq)
  tab23[,2:4] <- sapply(tab23[,2:4],function(x) percent(x*100,1))
  names(tab23) <- c("V1","V2","V3","V4")
  
  # Flextable
  outTab <- flextable(tab23) %>%
    set_header_labels("V1" = "Quartile", "V2" = "No", "V3" = "Yes", "V4" = "Sum") %>%
    add_header_lines(values=tabType) %>%
    merge_at(i=1, part="header") %>%
    align(i=1, part="header", align="center") %>%
    width(width=1.25)
  return(outTab)
}

# Checks for Latest Date, Checks for Processed Shape file, and Populates Date Dropdown Box
Date_Check <- function(KEY){

  f.curdate <- getCensus(name = "dec/responserate", vintage="2020", key = KEY,
                         vars = "RESP_DATE",
                         region = "us:*")
  
  currentDate <- f.curdate$RESP_DATE
  dateFName  <- "./datafiles/nation_Response_Data.csv"
  
  f.dateList <- read.csv(dateFName, header=TRUE,
                         colClasses = c(rep("character",3),rep("numeric",4))) %>% 
    select("RESP_DATE")
  f.dateList <- f.dateList %>% arrange(desc(RESP_DATE))
  dateList <- as.character(f.dateList$RESP_DATE)
  
  if(!(currentDate %in% f.dateList$RESP_DATE)) {
    update_data(currentDate, KEY)
    dateList <- c(currentDate, dateList)
  }
  
  #Checking for Shapefile...
#  if(!file.exists("./datafiles/tracts.Rdata")) {
#    
#  }
  
  return(dateList) 
  
}

# Updates data from Census API
update_data <- function(selDate, KEY) {

  #Create nation data for current date
  nationFName  <- "./datafiles/nation_Response_Data.csv"

  f.nation <- getCensus(name = "dec/responserate", vintage="2020", key = KEY,
                        vars = c("RESP_DATE",	"NAME","CRRALL",	"CRRINT", "DRRALL",	"DRRINT"),
                        region = "us:*")
  
  f.nation[,4:7] <- lapply(f.nation[,4:7],as.numeric)
  f.nation[,4:7] <- lapply(f.nation[,4:7], function(x) x/100)
  
  f.nationCum <- read.csv(nationFName, header=TRUE,
                          colClasses = c(rep("character",3),rep("numeric",4)))
  
  if(!(selDate %in% f.nationCum$RESP_DATE)) {
    f.nationCum <- bind_rows(f.nationCum, f.nation)
    write.csv(f.nationCum, nationFName, row.names = FALSE)
  } 
  
  
  
  #Create state data for current date
  stateFName  <- "./datafiles/state_Response_Data.csv"
  
  f.state <- getCensus(name = "dec/responserate", vintage="2020", key = KEY,
                       vars = c("RESP_DATE",	"NAME","CRRALL",	"CRRINT", "DRRALL",	"DRRINT"),
                       region = "state:*")
  
  f.state[,4:7] <- lapply(f.state[,4:7],as.numeric)
  f.state[,4:7] <- lapply(f.state[,4:7], function(x) x/100)
  f.state <- f.state %>% arrange(state)
  
  f.stateCum <- read.csv(stateFName, header=TRUE,
                         colClasses = c(rep("character",3),rep("numeric",4)))
  
  if(!(selDate %in% f.stateCum$RESP_DATE)) {
    f.stateCum <- bind_rows(f.stateCum, f.state)
    write.csv(f.stateCum, stateFName, row.names = FALSE)
  } 

  
  #Create county data for current date
  countyFName <- "./datafiles/county_Response_Data.csv"
  
  f.county <- getCensus(name = "dec/responserate", vintage="2020", key = KEY,
                        vars = c("RESP_DATE",	"NAME","CRRALL",	"CRRINT", "DRRALL",	"DRRINT"),
                        region = "county:*",
                        regionin = "state:08")
  
  f.county[,5:8] <- lapply(f.county[,5:8],as.numeric)
  f.county[,5:8] <- lapply(f.county[,5:8], function(x) x/100)
  f.county$NAME <- sub(", Colorado","",f.county$NAME)
  f.county <- f.county %>% arrange(county)
  
  f.countyCum <- read.csv(countyFName, header=TRUE, 
                          colClasses = c(rep("character",3),rep("numeric",4)))
  
  if(!(selDate %in% f.countyCum$RESP_DATE)) {
    f.countyCum <- bind_rows(f.countyCum, f.county[,2:8])
    write.csv(f.countyCum, countyFName, row.names = FALSE)
  } 
  
  
  
  #Create place data for current date
  placeFName <- "./datafiles/place_Response_Data.csv"
  
  f.place <- getCensus(name = "dec/responserate", vintage="2020", key = KEY, 
                       vars = c("RESP_DATE",	"NAME","CRRALL",	"CRRINT", "DRRALL",	"DRRINT"),
                       region = "place:*",
                       regionin = "state:08")
  
  f.place[,5:8] <- lapply(f.place[,5:8],as.numeric)
  f.place[,5:8] <- lapply(f.place[,5:8], function(x) x/100)
  f.place$NAME <- sub(", Colorado","",f.place$NAME)
  f.place <- f.place %>% arrange(place)
  
  f.placeCum <- read.csv(placeFName, header = TRUE,
                         colClasses = c(rep("character",4),rep("numeric",4)))
  
  if(!(selDate %in% f.placeCum$RESP_DATE)) {
    f.placeCum <- bind_rows(f.placeCum, f.place)
    write.csv(f.placeCum, placeFName, row.names = FALSE)
  } 
  
  
  #Create tract data for current date
  tractFName <- "./datafiles/tract_Response_Data.csv"
  
  f.tract <- getCensus(name = "dec/responserate", vintage="2020", key = KEY,
                       vars = c("RESP_DATE",	"NAME","CRRALL",	"CRRINT", "DRRALL",	"DRRINT"),
                       region = "tract:*",
                       regionin = "state:08")
  
  f.tract[,6:9] <- lapply(f.tract[,6:9],as.numeric)
  f.tract[,6:9] <- lapply(f.tract[,6:9], function(x) x/100)
  f.tract <- f.tract %>% arrange(county, tract)
  
  f.tractCum <- read.csv(tractFName, header = TRUE,
                         colClasses = c(rep("character",5),rep("numeric",4)))
  
  
  if(!(selDate %in% f.tractCum$RESP_DATE)) {
    f.tractCum <- bind_rows(f.tractCum, f.tract)
    write.csv(f.tractCum, tractFName, row.names = FALSE)
  } 

}



# Generates Leaflet Map

genMap <- function(selDate) {
  # Building Tract Map
  # Reading and coding up tract data into 20% catergories


  tractFName <- "./datafiles/tract_Response_Data.csv"
  f.tractCum <- read.csv(tractFName, header = TRUE,
                         colClasses = c(rep("character",5),rep("numeric",4)))  %>% 
                filter(RESP_DATE == selDate) %>%  
                mutate(CRRALL_Rank =  rank(CRRALL, na.last = TRUE, ties.method = "first"))   %>% 
                filter(CRRALL_Rank <= 100) %>%
                mutate(CRRALL_Rank = 101 - CRRALL_Rank)
  
  f.tractCum$GEOID20 <- paste0(f.tractCum$state, f.tractCum$county, f.tractCum$tract) 

  # Reading County Boundaries
 
  f.COcty <- st_read("datafiles/sr20_500k/county_bas20_sr_500k.shp")  %>%
    st_transform(crs="+init=epsg:4326") %>% filter(STATE == "08") 
  
  #Reading tract_bas20_sr_500k shapefile
  
   
  f.COShape <- st_read("datafiles/sr20_500k/tract_bas20_sr_500k.shp")  %>%
    st_transform(crs="+init=epsg:4326") %>% filter(STATE == "08") %>%
    mutate(GEOID20 = paste0(STATE, COUNTY, TRACT))
 
  
  f.COTractsM <- geo_join(f.COShape, f.tractCum, by = "GEOID20", how="left") %>% filter(!is.na(state))
  
  f.COTractsM$RespCat <- ifelse(f.COTractsM$CRRALL <= 0.159, 1,
                         ifelse(f.COTractsM$CRRALL <= 0.309, 2,
                         ifelse(f.COTractsM$CRRALL <= 0.409, 3,
                         ifelse(f.COTractsM$CRRALL <= 0.509, 4,
                         ifelse(f.COTractsM$CRRALL <= 0.569, 5,
                         ifelse(f.COTractsM$CRRALL <= 0.629, 6,
                         ifelse(f.COTractsM$CRRALL <= 0.689, 7,
                         ifelse(f.COTractsM$CRRALL <= 0.749, 8,
                         ifelse(f.COTractsM$CRRALL <= 0.859, 9,10)))))))))
 
  f.COTractsM$RespCat <- factor(f.COTractsM$RespCat, levels = c(1:10),
                                labels = c("00% to 15%", "16% to 30%",
                                           "31% to 40%", "41% to 50%",
                                           "51% to 56%", "57% to 62%",
                                           "63% to 68%", "69% to 74%",
                                           "75% to 85%", "86% to 100%"))
  f.COTractsM$VLabel <- paste0(f.COTractsM$NAME.y,"<br>Ranking: ",f.COTractsM$CRRALL_Rank," Response Rate: ",percent(f.COTractsM$CRRALL * 100,1))
  
  #Creating colors...
  cols <- c("chocolate4","chocolate3","chocolate2","chocolate1",
            "cyan1","cyan2","cyan3","cyan4","darkcyan","darkblue")
  
  pal <- colorFactor(cols, f.COTractsM$RespCat, n = 10)
  
  mapTitle <- "Cumulative Response Rate"
  
  outMap <- f.COcty %>%
    leaflet(width = "100%") %>%
    addTiles() %>% setView(lng = -105.358887, lat = 39.113014, zoom=7) %>%
    addPolygons(color="black", weight = 0.9) %>%
    addPolygons(data = f.COTractsM, stroke = TRUE, color="black", weight = 0.7, opacity = 0.4, smoothFactor = 0.2, fillOpacity = 0.4,
                fillColor = ~pal(f.COTractsM$RespCat), label =  lapply( f.COTractsM$VLabel, htmltools::HTML)
    ) %>%
    addLegend(pal=pal, values = ~f.COTractsM$RespCat, opacity = 1, title = mapTitle,
              position = "bottomright")  

    
  
 return(outMap) 

}

# Generates Report
genReport <- function(selDate) {

  ################
  # Creating Output 
  ################
  # National and Colorado Tab
  
  # National Tab
  nationFName  <- "./datafiles/nation_Response_Data.csv"
  f.nationCum <- read.csv(nationFName, header = TRUE,
                          colClasses = c(rep("character",3),rep("numeric",4)))
  
  f.nationCum[,4:7] <- lapply(f.nationCum[,4:7], function(x) percent(x*100,1))
  
  f.nationCRRALL <- f.nationCum %>% select(RESP_DATE, NAME, CRRALL) 
  f.nationCRRINT <- f.nationCum%>% select(RESP_DATE, NAME, CRRINT)
  
  
  # State tab
  stateFName  <- "./datafiles/state_Response_Data.csv"
  f.stateCum <- read.csv(stateFName, header=TRUE,
                         colClasses = c(rep("character",3),rep("numeric",4)))
  
  f.CO <- f.stateCum %>% filter(state == "08")
  f.COTrend <- f.CO
  f.CO[,4:7] <- lapply(f.CO[,4:7], function(x) percent(x*100,1))
  
  f.COCRRALL <- f.CO %>% select(RESP_DATE, NAME, CRRALL) 
  f.COCRRINT <- f.CO %>% select(RESP_DATE, NAME, CRRINT)
  
  f.CRRALL <- rbind(f.nationCRRALL, f.COCRRALL) %>% spread(NAME, CRRALL)
  
  f.CRRINT <- rbind(f.nationCRRINT, f.COCRRINT) %>%   spread(NAME, CRRINT)

  f.nationTab <- inner_join(f.CRRALL, f.CRRINT, by="RESP_DATE") %>% 
             arrange(desc(RESP_DATE))
  f.nationTab$date <- as.Date(f.nationTab$RESP_DATE,"%Y-%m-%d")
  SDate <- as.Date(selDate,"%Y-%m-%d")
 
  f.nationTab <- f.nationTab %>%  filter(date <= SDate) %>% filter(date >= SDate - 6)
  
  names(f.nationTab) <- c("V1", "V2","V3","V4","V5")
  
  
  # Shows last 7 days
  nationTAB <- flextable(f.nationTab[,1:5]) %>% 
    add_header(V2="Cumulative Total Response Rate", V4 = "Cumulative Internet Response Rate", top=TRUE) %>%
    set_header_labels(V1 = "Response Date", V2 = "Colorado", V3 = "United States",
                      V4 = "Colorado", V5 = "United States") %>%
    merge_at(i= 1,j=2:3, part = "header") %>%
    merge_at(i= 1,j=4:5, part = "header") %>%
    align(i = 1:2, part= "header", align="center") %>%
    align(j=1,part= "body", align="left") %>%
    align(j=2:5,part= "body", align="right") %>%
    autofit() %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(j=1:5, width=1.5) 
  
  # State Table, top 5 bottom 5 Colorado
  
  # State chart for latest date
  
  f.stateRank <- f.stateCum
  
  f.stateRank$CRRALL <- f.stateRank$CRRALL * 100
  f.stateN <- f.stateRank %>% filter(RESP_DATE == selDate) %>%
    mutate(CRRALL_Rank = 53 - rank(CRRALL, na.last = TRUE, ties.method = "first"),
           NAME = paste0(NAME," (",CRRALL_Rank,")")) 
  
  
  f.COloRank <- f.stateN %>% filter(state == "08")
  
  coloranktxt <- paste0("Currently, Colorado is the ",unlist(f.COloRank$CRRALL_Rank),"th ranked state out of 52 states (50 states + District of Columbia and Puerto Rico, Lower is better.).")
  
  
  f.stateCO <- f.stateN %>% filter( state == "08")
  f.stateTop <- f.stateN %>% filter(CRRALL_Rank <= 26)
  #f.stateBot <- f.stateN %>% filter(CRRALL_Rank >= 48) 
  
  #f.stateBar <- bind_rows( f.stateTop, f.stateBot) %>%
  #     select(CRRALL_Rank, NAME, CRRALL) %>% arrange(CRRALL_Rank)
  
  f.stateBar <-  f.stateTop %>%
    select(CRRALL_Rank, NAME, CRRALL) %>% arrange(CRRALL_Rank)
  
  stateLst <- unlist(f.stateBar$NAME)
  f.stateBar$NAME <- factor(f.stateBar$NAME, levels = rev(stateLst))
  f.stateBar$CO <- ifelse(grepl("Colorado",f.stateBar$NAME), "T","F")
  barCol <- c("#6EC4E8","#00953A")
  
  stateBar <- f.stateBar  %>%
    ggplot(aes(x= NAME, y = CRRALL, fill=CO)) +
    geom_bar(stat="identity") +
    geom_text(data = f.stateBar, 
              aes(x= NAME, y = 75, label=percent(CRRALL,1)),
              size = 3) +
    scale_y_continuous(limits=c(0,80), breaks=seq(0,80, by=10), labels = percent, expand = c(0,0)) +
    scale_fill_manual(values=barCol) +
    coord_flip() +
    labs(title = "Response Rate by State (Rank)" ,
         subtitle = selDate,
         caption = "Rank LE Colorado, Rank GE 48, and Colorado",
         x = "State",
         y= "Cumulative Total Response Rate") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text=element_text(size=9),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none")
  
  
  # County Ranking Table All Counties
  countyFName <- "./datafiles/county_Response_Data.csv"
  f.countyCum <- read.csv(countyFName, header=TRUE,
                          colClasses = c(rep("character",3),rep("numeric",4)))
  
  # Calculating percentage distribution
  
  f.countyMax <- f.countyCum
  
  
  f.countyMax <- f.countyMax %>% filter(RESP_DATE == selDate)
  
  f.countyMax$RespLeg <- ifelse(f.countyMax$CRRALL < 0.11, "00% to 10.9%", 
                                ifelse(f.countyMax$CRRALL < 0.21, "11.0% to 20.9%",
                                       ifelse(f.countyMax$CRRALL < 0.31, "21.0% to 30.9%",
                                              ifelse(f.countyMax$CRRALL < 0.41, "31.0% to 40.9%",
                                                     ifelse(f.countyMax$CRRALL < 0.51, "41.0% to 50.9%",
                                                            ifelse(f.countyMax$CRRALL < 0.61, "51.0% to 60.9%",  
                                                                   ifelse(f.countyMax$CRRALL < 0.71, "61.0% to 70.9%",
                                                                          ifelse(f.countyMax$CRRALL < 0.81, "71.0% to 80.9%",
                                                                                 ifelse(f.countyMax$CRRALL < 0.91, "81.0% to 90.9%", "91.0% to 100%")))))))))
  
  f.countyMax$RespLeg <- as.factor(f.countyMax$RespLeg)
  
  countyRespN <- as.data.frame(addmargins(table(f.countyMax$RespLeg)))
  countyRespP <- as.data.frame(addmargins(prop.table(table(f.countyMax$RespLeg))))
  countyresp <- inner_join(countyRespN, countyRespP, by="Var1")
  countyresp$Freq.y <- percent(countyresp$Freq.y *100,1)
  names(countyresp) <- c("Response Rate","Frequency","Percent")
  countyFreq <- flextable(countyresp) %>%
    align(i=1,part="header",align="center") %>%
    align(j = 1, part="body", align ="left") %>%
    align(j=2:3, part="body", align="right") %>%
    autofit() %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(j=1:3, width=1.5)
  
  
  f.countyMax$CRRALL_Rank <- 65 - rank(f.countyMax$CRRALL, na.last = TRUE, ties.method = "first") 
  
  f.countyMax <- f.countyMax %>% arrange(CRRALL_Rank, NAME)
  f.countyMax$CRRALL <- percent(f.countyMax$CRRALL*100,1)
  f.countyMax$CRRINT <- percent(f.countyMax$CRRINT*100,1)
  
  #Merging UL Area data
  f.CTYarea <- read.csv("./datafiles/County_CType2.csv", header=TRUE,
                        colClasses = c(rep("character",3),rep("numeric",3))) 
  f.CTYSum <- f.CTYarea %>%
    group_by(county,contact2) %>%
    summarize(housingunits = sum(housingunits),
              population = sum(population),
              SqMeters = sum(SqMeters)) 
  
  f.CTYSumCTY <- f.CTYarea %>%
    group_by(county) %>%
    summarize(housingunits = sum(housingunits),
              population = sum(population),
              SqMeters = sum(SqMeters))
  
  f.CTYSum2 <- f.CTYSum %>%
    gather(measure, value, housingunits:SqMeters) %>%
    spread(contact2,value) %>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  
  names(f.CTYSum2) <- c("county","measure","HDL","HDP","Other","UL")
  f.CTYSum3 <- f.CTYSum2 %>%
    group_by(county,measure) %>%
    mutate(total = sum(HDL, HDP, Other, UL),
           HDLPCT = percent(HDL/total *100,1),
           HDPPCT = percent(HDP/total * 100,1),
           OthPCT = percent(Other/total * 100,1),
           ULPCT =  percent(UL/total * 100,1))
  
  
  f.CTYSumHU <- f.CTYSum3 %>% filter(measure == "housingunits")
  names(f.CTYSumHU)[3:11] <-sapply(names(f.CTYSumHU)[3:11], function(x) paste0(x,"_HU"))
  f.CTYSumPop <- f.CTYSum3 %>% filter(measure == "population")
  names(f.CTYSumPop)[3:11] <-sapply(names(f.CTYSumPop)[3:11], function(x) paste0(x,"_Pop"))
  
  
  f.countyMaxUL <- inner_join(f.countyMax,f.CTYSumHU, by= "county") %>%
    inner_join(.,f.CTYSumPop, by="county")
  f.countyMaxUL$total_HU <- format(f.countyMaxUL$total_HU,digits=0, big.mark=",",scientific=FALSE)
  f.countyMaxUL$total_Pop <- format(f.countyMaxUL$total_Pop,digits=0, big.mark=",",scientific=FALSE)
  
  countyTAB <- flextable(f.countyMaxUL[,c(2,15,25,19,29,4,5,9)]) %>% 
    set_header_labels(NAME = "County", 
                      total_HU = "Housing Units in County",
                      total_Pop = "Estimated Population in County",
                      CRRALL = "Cumulative Total Response Rate", CRRINT = "Cumulative Internet Response Rate", 
                      "ULPCT_HU" = "Percent of Housing Units in Update/ Leave Blocks",
                      "ULPCT_Pop" =  "Percent of Population in Update/ Leave Blocks",
                      "CRRALL_Rank" = "County Ranking") %>%
    align(i=1, part= "header", align="center") %>%
    align(j=1,part= "body", align="left") %>%
    align(j=2:8,part= "body", align="right") %>%
    width(j=1, width=1.4) %>%
    width(j=2:5, width=0.9) %>%
    width(j=6:7, width=1) %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(j=8,width = 0.8)
  
  #Place Ranking Table  This Is the first and last quartile..
  
  placeFName <- "./datafiles/place_Response_Data.csv"
  f.placeCum <- read.csv(placeFName, header = TRUE,
                         colClasses = c(rep("character",4),rep("numeric",4)))
  
  f.placeMax <- f.placeCum

  f.placeN <- f.placeMax %>% filter(RESP_DATE == selDate) %>%
    mutate(CRRALL = CRRALL * 100, 
           CRRINT = CRRINT * 100, 
          CRRALL_Rank = 271 - rank(CRRALL, na.last = TRUE, ties.method = "first")) 
  
  
  f.placeNames <- read.csv("./datafiles/places.csv", header = TRUE,
                           colClasses = c(rep("character",4)))
  
  f.placeN <- inner_join(f.placeNames,f.placeN, by="place") 
  
  f.placehi <- f.placeN %>% 
    arrange(CRRALL_Rank,NAME) %>%
    mutate(CRRALL = percent(CRRALL,1),
           CRRINT = percent(CRRINT,1)) %>%
    select(county, NAME, CRRALL, CRRINT, CRRALL_Rank)
  
  
  
  
  names(f.placehi) <- c("V1", "V2", "V3","V4", "V5")
  
  placeTABHI <- flextable(f.placehi) %>% 
    set_header_labels(V1 = "County", V2 = "Place/ Municipality", V3 = "Cumulative Total Response Rate", 
                      V4 = "Cumulative Internet Response Rate", V5="Rank") %>%
    align(i = 1, part= "header", align="center") %>%
    align(j=1:2, part= "body", align="left") %>%
    align(j=3:5,part= "body", align="right") %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(width=1.5)
  
  f.placeSum <- f.placeN %>%
    summarize(V1 = percent(min(CRRALL),1),
              V2 = percent(quantile(CRRALL, .25),1),
              V3 = percent(quantile(CRRALL, .50),1),
              V4 = percent(quantile(CRRALL, .75),1),
              V5 = percent(max(CRRALL),1),
              V6 = percent(mean(CRRALL),1),
              V7 = percent(sd(CRRALL),1)
    )
  
  
  placeSUM <- flextable(f.placeSum) %>%
    set_header_labels(V1 = "Minimum", V2 = "First Quartile", 
                      V3 = "Median", V4 = "Third Quartile",
                      V5 = "Maximum", V6 = "Average", V7 = "Standard Deviation") %>%
    align(i = 1, part= "header", align="center") %>%
    align(i=1, part= "body", align="right") %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(width=1)
  
 f.placeN$RespLeg <- ifelse(f.placeN$CRRALL < 11, "00% to 10.9%", 
                                ifelse(f.placeN$CRRALL < 21, "11.0% to 20.9%",
                                       ifelse(f.placeN$CRRALL < 31, "21.0% to 30.9%",
                                              ifelse(f.placeN$CRRALL < 41, "31.0% to 40.9%",
                                                     ifelse(f.placeN$CRRALL < 51, "41.0% to 50.9%",
                                                            ifelse(f.placeN$CRRALL < 61, "51.0% to 60.9%",  
                                                                   ifelse(f.placeN$CRRALL < 71, "61.0% to 70.9%",
                                                                          ifelse(f.placeN$CRRALL < 81, "71.0% to 80.9%",
                                                                                 ifelse(f.placeN$CRRALL < 91, "81.0% to 90.9%", "91.0% to 100%")))))))))
  
  f.placeN$RespLeg <- as.factor(f.placeN$RespLeg)
  placeRespN <- as.data.frame(addmargins(table(f.placeN$RespLeg)))
  placeRespP <- as.data.frame(addmargins(prop.table(table(f.placeN$RespLeg))))
  placeresp <- inner_join(placeRespN, placeRespP, by="Var1")
  placeresp$Freq.y <- percent(placeresp$Freq.y *100,1)
  names(placeresp) <- c("Response Rate","Frequency","Percent")
  placeFreq <- flextable(placeresp) %>%
    align(i=1,part="header",align="center") %>%
    align(j = 1, part="body", align ="left") %>%
    align(j=2:3, part="body", align="right") %>%
    autofit() %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(j=1:3, width=1.5)
  
  
  #Tract Charts...

  tractFName <- "./datafiles/tract_Response_Data.csv"
  f.tractCum <- read.csv(tractFName, header=TRUE,
                         colClasses = c(rep("character",5),rep("numeric",4))) %>%
              filter(RESP_DATE == selDate)

  f.tractCum100 <- f.tractCum %>%
      mutate(GEOID20 = paste0(f.tractCum$state, f.tractCum$county, f.tractCum$tract),
             CRRALL_Rank = rank(CRRALL, na.last = TRUE, ties.method = "first"))   %>% 
    filter(CRRALL_Rank <= 100) %>%
    mutate(CRRALL_Rank = 101 - CRRALL_Rank) %>%
    arrange(desc(CRRALL_Rank))
  f.tractCum100[,6:9] <- sapply(f.tractCum100[,6:9],function(x) percent(x*100,1))           

  # Tract Response by Quartile
  
  f.TrQuartile <- f.tractCum %>%
    summarize(MinVal = percent(min(CRRALL)*100),
              Q1 = percent(quantile(CRRALL,0.25)*100),
              Q2 = percent(quantile(CRRALL,0.50)*100),
              Q3 = percent(quantile(CRRALL,0.75)*100),
              MaxVal = percent(max(CRRALL)*100),
              MeanVal = percent(mean(CRRALL)*100),
              sdVal = percent(sd(CRRALL)*100))
  
  # Output Tables
  TRQuantTab <- flextable(f.TrQuartile) %>%
    set_header_labels("MinVal" = "Minimum", "Q1" = "First Quartile",
                      "Q2" = "Median", "Q3" = "Third Quartile",
                      "MaxVal" = "Maximum","MeanVal" ="Average", "sdVal" = "Standard Deviation") %>%
    align(i = 1, part= "header", align="center") %>%
    align(i = 1,part= "body", align="right") %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(width=1)
  
  TR100TAB <- flextable(f.tractCum100[,c(4,6:9,11)]) %>%
    set_header_labels("NAME" = "Tract and County", 
                      "CRRALL" = "Cumulative Total Response Rate", 
                      "CRRINT" = "Cumulative Internet Response Rate",
                      "DRRALL" = "Daily Total Response Rate", 
                      "DRRINT" = "Daily Internet Response Rate",
                      "CRRALL_Rank"="Rank") %>%
    align(i = 1, part= "header", align="center") %>%
    align(j=1, part="body", align = "left") %>%
    align(j = 2:6,part= "body", align="right") %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(j = 1, width = 3) %>%
    width(j = 2:6,width = 1) 
  

  
  #Preparing output document
  titleTxt <- paste0("Colorado Census Response Rate Report for ",selDate)
  
  
  reportDoc <- read_docx() %>% 
    body_add_par(titleTxt, style = "heading 1") %>% 
    body_add_par("", style="Normal") %>%
    body_add_par("State Results", style= "heading 2") %>%
    body_add_par("", style="Normal") %>% 
    body_add_par("State and National Trend since Beginning", style= "heading 3")  %>%
    body_add_flextable(value = nationTAB) %>% 
    body_add_par("", style="Normal") %>%
    body_add_par("State Comparison Chart", style="heading 3") %>%
    body_add_par("This chart compares the top 5 states, the bottom 5 states, and Colorado in terms of the Cumulative Total Response Rate.", style="Normal") %>%
    body_add_par(coloranktxt, style="Normal") %>%
    body_add_par("", style="Normal") %>%
    body_add_gg(value=stateBar, width = 6, height = 4, res = 300) %>%
    body_add_par("County Results", style= "heading 2") %>%
    body_add_par("", style="Normal") %>% 
    body_add_par("County Summary Table", style="heading 3") %>%
    body_add_par("", style="Normal") %>%
    body_add_flextable(value = countyFreq) %>% 
    body_add_par("", style="Normal") %>%
    body_add_par("County Rankings by Response Rate", style="heading 3") %>%
    body_add_par("", style="Normal") %>%
    body_add_flextable(value = countyTAB) %>%
    body_add_break(pos="after") %>%
    body_add_par("Place Results", style= "heading 2") %>%
    body_add_par("", style="Normal") %>% 
    body_add_par("Municipalities/Places Summary Table", style="heading 3") %>%
    body_add_par("", style="Normal") %>%
    body_add_par("Distribution of Municipalities/Places", style="Normal") %>% 
    body_add_flextable(value = placeSUM) %>%  
    body_add_par("", style="Normal") %>%
    body_add_par("Frequency Table", style="Normal") %>% 
    body_add_flextable(value = placeFreq) %>%  
    body_add_par("", style="Normal") %>%
    body_add_par("Municipalities/Places by Cumulative Total Response Rate", style="Normal") %>%
    body_add_flextable(value = placeTABHI) %>%
    body_add_break(pos="after") %>%
    body_add_par("Tract-level Results", style= "heading 2") %>%
    body_add_par("", style="Normal") %>% 
    body_add_par("Tract Summary Table", style="heading 3") %>%
    body_add_par("", style="Normal") %>%
    body_add_flextable(value = TRQuantTab) %>% 
    body_add_par("Response Rate by For 100 Lowest Responding Census Tracts", style="heading 3") %>% 
    body_add_par("", style="Normal") %>%
    body_add_flextable(value = TR100TAB)
   
  
  return(reportDoc)
}
