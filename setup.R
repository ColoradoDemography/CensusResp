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
library(rgdal)
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

# Populates Date Dropdown Box
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
                         colClasses = c(rep("character",5),rep("numeric",4))) %>% 
                filter(RESP_DATE == selDate)
  f.tractCum$GEOID20 <- paste0(f.tractCum$state, f.tractCum$county, f.tractCum$tract) 
  
  #Reading tract_bas20_sr_500k shapefile
  
  f.shape <- readOGR(dsn = "./datafiles/sr20_500k",
                     layer = "tract_bas20_sr_500k",
                     verbose = FALSE)
  
  
  f.COShape <- subset(f.shape,f.shape$STATE == "08")
  f.COShape <- spTransform(f.COShape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  f.COShape$GEOID20 <- paste0(f.COShape$STATE, f.COShape$COUNTY, f.COShape$TRACT)
  
  
  f.COTractsM <- geo_join(f.COShape, f.tractCum, by = "GEOID20", how="left") 
  f.COTractsM <- subset(f.COTractsM,!is.na(f.COTractsM$state))
  
  f.COTractsM$RespCat <- (f.COTractsM$CRRALL%/%0.10) + 1
  
  f.COTractsM$RespLeg <- ifelse(f.COTractsM$RespCat  == 1, "00% to 10%", 
                                ifelse(f.COTractsM$RespCat  == 2, "11% to 20%",
                                       ifelse(f.COTractsM$RespCat  == 3, "21% to 30%",
                                              ifelse(f.COTractsM$RespCat  == 4, "31% to 40%",
                                                     ifelse(f.COTractsM$RespCat == 5, "41% to 50%",
                                                            ifelse(f.COTractsM$RespCat == 6, "51% to 60%",  
                                                                   ifelse(f.COTractsM$RespCat == 7, "61% to 70%",
                                                                          ifelse(f.COTractsM$RespCat == 8, "71% to 80%",
                                                                                 ifelse(f.COTractsM$RespCat == 9, "81% to 90%", "91% to 100%")))))))))
  
  f.COTractsM$RespLeg <- as.factor(f.COTractsM$RespLeg)
  f.COTractsM$VLabel <- paste0(f.COTractsM$NAME.1," ",percent(f.COTractsM$CRRALL * 100,1))
  
  pal <- colorFactor("YlGnBu", f.COTractsM$RespLeg)
  
  mapTitle <- "Cumulative Response Rate"
  
  outMap <- leaflet(f.COTractsM) %>%
    addTiles() %>% setView(lng = -105.358887, lat = 39.113014, zoom=6) %>%
    addPolygons(stroke = TRUE, color="black", weight = 0.7, opacity = 0.4, smoothFactor = 0.2, fillOpacity = 0.4,
                fillColor = ~pal(RespLeg), label =  ~htmlEscape(VLabel)
    ) %>%
    addLegend(pal=pal, values = ~RespLeg, opacity = 1, title = mapTitle,
              position = "topright")
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
  f.stateTop <- f.stateN %>% filter(CRRALL_Rank <= 5)
  f.stateBot <- f.stateN %>% filter(CRRALL_Rank >= 48) 
  
  f.stateBar <- bind_rows(f.stateCO, f.stateTop, f.stateBot) %>%
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
              size = 4) +
    scale_y_continuous(limits=c(0,80), breaks=seq(0,80, by=10), labels = percent, expand = c(0,0)) +
    scale_fill_manual(values=barCol) +
    coord_flip() +
    labs(title = "Response Rate by State (Rank)" ,
         subtitle = "Top 5 Ranked States, Bottom 5 Ranked States and Colorado",
         caption = paste0("State Demography Office.  Data for: ",selDate),
         x = "State",
         y= "Cumulative Total Response Rate") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text=element_text(size=12),
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
  
  f.countyMax$RespCat <- (f.countyMax$CRRALL%/%0.10) + 1
  
  f.countyMax$RespLeg <- ifelse(f.countyMax$RespCat  == 1, "00% to 10%", 
                                ifelse(f.countyMax$RespCat  == 2, "11% to 20%",
                                       ifelse(f.countyMax$RespCat  == 3, "21% to 30%",
                                              ifelse(f.countyMax$RespCat  == 4, "31% to 40%",
                                                     ifelse(f.countyMax$RespCat == 5, "41% to 50%",
                                                            ifelse(f.countyMax$RespCat == 6, "51% to 60%",  
                                                                   ifelse(f.countyMax$RespCat == 7, "61% to 70%",
                                                                          ifelse(f.countyMax$RespCat == 8, "71% to 80%",
                                                                                 ifelse(f.countyMax$RespCat == 9, "81% to 90%", "91% to 100%")))))))))
  
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
  
  
  f.countyMaxUL <- inner_join(f.countyMax,f.CTYSumCTY, by = "county") %>%
                   inner_join(., f.CTYSumHU, by= "county") %>%
                   inner_join(.,f.CTYSumPop, by="county")
  f.countyMaxUL$housingunits <- format(f.countyMaxUL$housingunits,big.mark=",",scientific=FALSE)
  f.countyMaxUL$population <- format(f.countyMaxUL$population,big.mark=",",scientific=FALSE)
  
  
  countyTAB <- flextable(f.countyMaxUL[,c(2,11,12,23,33,4,5,10)]) %>% 
    set_header_labels(NAME = "County", 
                      housingunits = "Housing Units in County",
                      population = "Estimated Population in County",
                      ULPCT_HU = "Percent of Housing Units in Update/ Leave Blocks",
                      ULPCT_Pop =  "Percent of Population in Update/ Leave Blocks",
                      CRRALL = "Cumulative Total Response Rate", 
                      CRRINT = "Cumulative Internet Response Rate", 
                      CRRALL_Rank = "County Ranking") %>%
    align(i=1, part= "header", align="center") %>%
    align(j=1,part= "body", align="left") %>%
    align(j=2:8,part= "body", align="right") %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    autofit() %>%
    width(width=1)  
    
  

  #Place Ranking Table  outputs the first 25 and the last 50
  placeFName <- "./datafiles/place_Response_Data.csv"
  f.placeCum <- read.csv(placeFName,header=TRUE,
                         colClasses = c(rep("character",4),rep("numeric",4)))
  
  f.placeMax <- f.placeCum
  
  f.placeN <- f.placeMax %>% filter(RESP_DATE == selDate) %>%
    mutate(CRRALL_Rank = 271 - rank(CRRALL, na.last = TRUE, ties.method = "first")) 
  f.placeN$CRRALL <- f.placeN$CRRALL * 100
  
  f.placeNames <- read.csv("./datafiles/places.csv", header=TRUE,
                           colClasses=c(rep("character",4)))
  f.placeN <- inner_join(f.placeNames[,c(2,4)],f.placeN, by="place") 
  
  f.placehi <- f.placeN %>% filter(CRRALL_Rank <= 25) %>% arrange(CRRALL_Rank,NAME) %>%
    mutate(CRRALL = percent(CRRALL,1),
           CRRINT = percent(CRRINT * 100,1)) %>%
    select(county, NAME, CRRALL, CRRINT, CRRALL_Rank)
  f.placelow <- f.placeN %>% filter(CRRALL_Rank >= 220) %>% arrange(desc(CRRALL_Rank),NAME) %>%
    mutate(CRRALL = percent(CRRALL,1),
           CRRINT = percent(CRRINT * 100,1)) %>%
    select(county, NAME, CRRALL, CRRINT, CRRALL_Rank) 
  
   names(f.placehi) <- c("V1", "V2", "V3","V4", "V5")
  names(f.placelow) <- c("V1", "V2", "V3", "V4","V5")
  
  
  placeTABHI <- flextable(f.placehi) %>% 
    set_header_labels(V1 = "County", V2 = "Place/ Municipality", V3 = "Cumulative Total Response Rate", 
                      V4 = "Cumulative Internet Response Rate", V5="Statewide Rank") %>%
    align(i = 1, part= "header", align="center") %>%
    align(j=1:2, part= "body", align="left") %>%
    align(j=3:5,part= "body", align="right") %>%
    border(border.top = fp_border(color = "black"),
           border.bottom = fp_border(color = "black"),
           border.left = fp_border(color = "black"),
           border.right = fp_border(color = "black"), part="all") %>%
    width(width=1.5)
  
  
  placeTABLOW <- flextable(f.placelow) %>% 
    set_header_labels(V1 = "County", V2 = "Place/ Municipality", V3 = "Cumulative Total Response Rate", 
                      V4 = "Cumulative Internet Response Rate", V5="Statewide Rank") %>%
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
  
  f.placeN$RespCat <- ((f.placeN$CRRALL/100)%/%0.10) + 1
  
  f.placeN$RespLeg <- ifelse(f.placeN$RespCat  == 1, "00% to 10%", 
                             ifelse(f.placeN$RespCat  == 2, "11% to 20%",
                                    ifelse(f.placeN$RespCat  == 3, "21% to 30%",
                                           ifelse(f.placeN$RespCat  == 4, "31% to 40%",
                                                  ifelse(f.placeN$RespCat == 5, "41% to 50%",
                                                         ifelse(f.placeN$RespCat == 6, "51% to 60%",  
                                                                ifelse(f.placeN$RespCat == 7, "61% to 70%",
                                                                       ifelse(f.placeN$RespCat == 8, "71% to 80%",
                                                                              ifelse(f.placeN$RespCat == 9, "81% to 90%", "91% to 100%")))))))))
  
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
    width(j=1:3, width=1.5)
  
  
  #Tract Charts...
  
  tractFName <- "./datafiles/tract_Response_Data.csv"
  f.tractCum <- read.csv(tractFName, header=TRUE,
                         colClasses = c(rep("character",5),rep("numeric",4)))
  f.tractCum$GEOID20 <- paste0(f.tractCum$state, f.tractCum$county, f.tractCum$tract) 
  
  # CRRALL by Contact Type
  #Contact Type
  contactType <- "./datafiles/Tract_Pop_Housing_by_Contact_Type.csv"
  f.contact <- read.csv(contactType, header=TRUE,
                        colClasses = c(rep("character",4),rep("numeric",5))) %>% 
                filter(CType2 != "Other")
  
  f.contactSum <- f.contact %>%
    group_by(TRACTGEOID) %>%
    summarize(HDL = sum(HDL),
              HDP = sum(HDP),
              UL = sum(UL))
  
  f.contactSum$GEOID10 <- f.contactSum$TRACTGEOID
  
  #Tract CrossWALK
  relWalk <-"./datafiles/rr_tract_rel/rr_tract_rel.txt"
  
  f.trrel <- read.csv(relWalk, header= TRUE, 
                      colClasses = c(rep("character",4), rep("numeric",2),
                                     rep("character",4), rep("numeric",12)))  %>% filter(STATEFP10 == "08")
  
  
  #Joining to tracts
  
  f.tractRel <- inner_join(f.tractCum,f.trrel, by= "GEOID20")
  
  f.TrContact <- inner_join(f.contactSum, f.tractRel, by="GEOID10" )
  f.TrContact$CRRALL <- f.TrContact$CRRALL * 100
  
  
  f.TrTrend <- f.TrContact
  f.TrContact <- f.TrContact %>% filter(RESP_DATE == selDate)
  f.TrContact <- f.TrContact[!duplicated(f.TrContact$NAME),]
  
  # Update/Leave
  f.ulplot <- f.TrContact %>%
    group_by(UL) %>%
    summarize(meanALL = mean(CRRALL),
              sdALL = sd(CRRALL),
              seALL = sdALL/sqrt(length(CRRALL)),
              minALL = meanALL - seALL,
              maxALL =  meanALL + seALL,
              meanINT = mean(CRRINT),
              sdINT = sd(CRRINT),
              seINT = sdINT/sqrt(length(CRRINT)),
              minINT = meanINT - seINT,
              maxINT =  meanINT + seINT)
  
  f.ulplot$UL <- as.factor(f.ulplot$UL)
  
  ALLULplot <- f.ulplot %>% ggplot() +
    aes(x = UL, y = meanALL, fill=UL) +
    geom_bar(stat="identity") +
    geom_text(aes(x= UL, y= 70, label=percent(meanALL,1)), size = 4) +
    scale_y_continuous(limits=c(0,80), breaks=seq(0,80, by=10), labels = percent, expand = c(0,0)) +
    scale_x_discrete(breaks=c(0,1), labels=c("No","Yes")) +
    scale_fill_manual(values=c("grey60","cadetblue3")) +
    labs(title = "Average Cumulative Total Response Rate" ,
         subtitle = "Tracts containing Update/Leave Blocks",
         caption = paste0("State Demography Office.  Data for: ",selDate),
         x = "Update/Leave",
         y= "Cumulative Total Response Rate") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text=element_text(size=12),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none")
  
  
  
  #Home Delivery Letter
  f.HDLplot <- f.TrContact %>%
    group_by(HDL) %>%
    summarize(meanALL = mean(CRRALL),
              sdALL = sd(CRRALL),
              seALL = sdALL/sqrt(length(CRRALL)),
              minALL = meanALL - seALL,
              maxALL =  meanALL + seALL,
              meanINT = mean(CRRINT),
              sdINT = sd(CRRINT),
              seINT = sdINT/sqrt(length(CRRINT)),
              minINT = meanINT - seINT,
              maxINT =  meanINT + seINT)
  
  f.HDLplot$HDL <- as.factor(f.HDLplot$HDL)
  
  ALLHDLplot <- f.HDLplot %>% ggplot() +
    aes(x = HDL, y = meanALL, fill=HDL) +
    geom_bar(stat="identity") +
    geom_text(aes(x= HDL, y= 70, label=percent(meanALL,1)), size = 4) +
    scale_y_continuous(limits=c(0,80), breaks=seq(0,80, by=10), labels = percent, expand = c(0,0)) +
    scale_x_discrete(breaks=c(0,1), labels=c("No","Yes")) +
    scale_fill_manual(values=c("grey60","cadetblue3")) +
    labs(title = "Average Cumulative Total Response Rate" ,
         subtitle = "Tracts containing Home Delivery Letter (Internet First) Blocks",
         caption = paste0("State Demography Office.  Data for: ", selDate),
         x = "Home Delivery Letter (Internet First)",
         y= "Cumulative Total Response Rate") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text=element_text(size=12),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none")
  
  
  
  #Home Delivery Package
  f.HDPplot <- f.TrContact %>%
    group_by(HDP) %>%
    summarize(meanALL = mean(CRRALL),
              sdALL = sd(CRRALL),
              seALL = sdALL/sqrt(length(CRRALL)),
              minALL = meanALL - seALL,
              maxALL =  meanALL + seALL,
              meanINT = mean(CRRINT),
              sdINT = sd(CRRINT),
              seINT = sdINT/sqrt(length(CRRINT)),
              minINT = meanINT - seINT,
              maxINT =  meanINT + seINT)
  
  f.HDPplot$HDP <- as.factor(f.HDPplot$HDP)
  
  ALLHDPplot <- f.HDPplot %>% ggplot() +
    aes(x = HDP, y = meanALL, fill=HDP) +
    geom_bar(stat="identity") +
    geom_text(aes(x= HDP, y= 70, label=percent(meanALL,1)), size = 4) +
    scale_y_continuous(limits=c(0,80), breaks=seq(0,80, by=10), labels = percent, expand = c(0,0)) +
    scale_x_discrete(breaks=c(0,1), labels=c("No","Yes")) +
    scale_fill_manual(values=c("grey60","cadetblue3")) +
    labs(title = "Average Cumulative Total Response Rate" ,
         subtitle = "Tracts containing Home Delivery Package (Internet Choice) Blocks",
         caption = paste0("State Demography Office.  Data for: ", selDate),
         x = "Home Delivery Package (Internet Choice)",
         y= "Cumulative Total Response Rate") +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text=element_text(size=12),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none")
  
  
  #Trend by Contact Type
  f.StTrend <- f.COTrend %>% 
    mutate(NAME2 = "Statewide",
           CRRALL = CRRALL * 100) %>%
    select(NAME2, RESP_DATE, CRRALL)
  
  f.AllTrend <- f.TrTrend %>% 
    mutate(NAME2 = "All Tracts") %>%
    select(NAME2, RESP_DATE, CRRALL)
  
  f.HDLTrend <- f.TrTrend %>% filter(HDL == 1) %>%
    mutate(NAME2 = "Home Delivery Letter Tracts (Internet First)") %>%
    select(NAME2, RESP_DATE, CRRALL)
  
  f.HDPTrend <- f.TrTrend %>% filter(HDP == 1) %>%
    mutate(NAME2 = "Home Delivery Package Tracts (Internet Choice)") %>%
    select(NAME2, RESP_DATE, CRRALL)
  
  f.ULTrend <- f.TrTrend %>% filter(UL == 1) %>%
    mutate(NAME2 = "Update/Leave Tracts") %>%
    select(NAME2, RESP_DATE, CRRALL)
  
  f.TrendTR  <- bind_rows(f.StTrend, f.AllTrend, f.HDLTrend, f.HDPTrend,f.ULTrend)
  f.TrendTR$NAME2 <- factor(f.TrendTR$NAME2, levels= c("Statewide", "All Tracts",
                                                       "Home Delivery Letter Tracts (Internet First)", 
                                                       "Home Delivery Package Tracts (Internet Choice)",
                                                       "Update/Leave Tracts"))


    f.TrendTR$date <- as.Date(f.TrendTR$RESP_DATE,"%Y-%m-%d")
 
  
  
  f.TrendAvg <- f.TrendTR %>% filter(date <= SDate) %>%
    group_by(NAME2, date) %>%
    summarize(MeanRR = mean(CRRALL))
  
  minDate <- min(f.TrendAvg$date)
  maxDate <- max(f.TrendAvg$date)
  
  TrendAvgLine <- f.TrendAvg  %>%
    ggplot(aes(x=date,y=MeanRR, color=NAME2)) +
    geom_line(size=1.2) +
    scale_x_date(limits = as.Date(c(minDate,maxDate)),breaks= seq(as.Date(minDate),as.Date(maxDate), by=2)) +
    scale_y_continuous(limits=c(0,60), breaks=seq(0,60, by=5), labels = percent, expand = c(0,0)) +
    scale_color_manual(values=c('gray','forestgreen', 'red2', 'orange', 'blue'), 
                       name = "Contact Type") +
    labs(title = "Response Rate Trend by Conttact Type" ,
         subtitle = "Statewide and by Tracts containing Contact Type blocks",
         caption = paste0("State Demography Office.  Data for: ",selDate),
         x = "Date",
         y= "Average Cumulative Total Response Rate") +
    guides(color=guide_legend(nrow=2,byrow=TRUE)) +
    theme(plot.title = element_text(hjust = 0.5, size=16),
          axis.text=element_text(size=10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", colour = "gray50"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "bottom")
  
  
  
  # Tract Response by Quartile
  
  f.TrQuartile <- f.TrContact %>%
    summarize(MinVal = min(CRRALL),
              Q1 = quantile(CRRALL,0.25),
              Q2 = quantile(CRRALL,0.50),
              Q3 = quantile(CRRALL,0.75),
              MaxVal = max(CRRALL),
              MeanVal = mean(CRRALL),
              sdVal = sd(CRRALL))
  
  f.TrContact$quartiles <- ifelse(f.TrContact$CRRALL <= f.TrQuartile$Q1,1,
                                  ifelse(f.TrContact$CRRALL <= f.TrQuartile$Q2,2,
                                         ifelse(f.TrContact$CRRALL <= f.TrQuartile$Q3,3,4)))
  f.TrContact$quartiles <- as.factor(f.TrContact$quartiles)
  
  f.TrQuartile2 <- sapply(f.TrQuartile, function(x) percent(x,1))
  
  f.TrQ <- data.frame(matrix(unlist(f.TrQuartile2), nrow=1, byrow=T),stringsAsFactors=FALSE)
  
  # Frequency by Quartile
  
  f.TRFreq <- as.data.frame(addmargins(table(f.TrContact$quartiles)))
  
  
  f.TrLow <- f.TrContact %>% filter(quartiles == 1) %>%
    arrange(CRRALL, county, tract) %>%
    mutate(CRRALL = percent(CRRALL,1))
  
  # Output Tables
  TRQuantTab <- flextable(f.TrQ) %>%
    set_header_labels("X1" = "Minimum", "X2" = "First Quartile",
                      "X3" = "Median", "X4" = "Third Quartile",
                      "X5" = "Maximum","X6" ="Average", "X7" = "Standard Deviation") %>%
    align(i = 1, part= "header", align="center") %>%
    align(i = 1,part= "body", align="right") %>%
    width(width=0.8)
  
  TRQuantFREQ <- flextable(f.TRFreq) %>%
    set_header_labels("Var1" = "Quartile", "Freq" = "Frequency") %>%
    align(i = 1, part= "header", align="center") %>%
    align(j=1, part="body", align = "left") %>%
    align(j = 2,part= "body", align="right") %>%
    width(width=1)
  
  TRHDLQTAB <- rowTab(f.TrContact$quartiles,f.TrContact$HDL,"Hand Delivery Letter (Internet First)")
  TRHDPQTAB <- rowTab(f.TrContact$quartiles,f.TrContact$HDP,"Hand Delivery Package (Internet Choice)")
  TRULQTAB <- rowTab(f.TrContact$quartiles,f.TrContact$UL,"Update/Leave")
 
  
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
    body_add_par("This chart compares the top 5 states, the bottom 5 states, and Colorado in terms of the Cumulative Total Resonse Rate.", style="Normal") %>%
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
    body_add_par("Place Comparison Tables", style="heading 3") %>%
    body_add_par("", style="Normal") %>%
    body_add_par("Distribution of Places", style="Normal") %>% 
    body_add_flextable(value = placeSUM) %>%  
    body_add_par("", style="Normal") %>%
    body_add_par("Frequency Table", style="Normal") %>% 
    body_add_flextable(value = placeFreq) %>%  
    body_add_par("", style="Normal") %>%
   # body_add_par("High and Low Response Places", style="heading 3") %>%  
   # body_add_par("This table extracts response rate for places using the Cumulative Total Response Rate. ", style="Normal") %>%
   # body_add_par("", style="Normal") %>%
   # body_add_par("25 Places/Municipalities with the Highest Cumulative Total Response Rate", style="Normal") %>%
   # body_add_flextable(value = placeTABHI) %>%
   # body_add_break(pos="after") %>%
   # body_add_par("50 Places/Municipalities with the Lowest Cumulative Total Response Rate", style="Normal") %>%
   # body_add_flextable(value = placeTABLOW) %>%
    body_add_break(pos="after") %>%
    body_add_par("Tract-level Results", style= "heading 2") %>%
    body_add_par("", style="Normal") %>% 
    body_add_par("Response Rate by Contact Type for Census Tracts", style="heading 3") %>% 
    body_add_par("", style="Normal") %>%
    body_add_par("Contact type (through the Colorado Census Contact Type Map) is assigned at the block level, but response rate data is available to the Tract Level.
   Tracts can have more than one type of 'Contact Type.'  e.g., any tract with at least one 'Update/Leave' block is classified as an 'Update Leave' tract.
   The charts show three types of contact type: 'Home Delivery Letter (Internet First)', 'Home Delivery Package (Internet Choice)' and 'Update/Leave'.", style="Normal") %>%
    body_add_par("", style="Normal") %>%
    body_add_par("Distribution of Response Rate by Tracts", style="Normal") %>%
    body_add_flextable(value = TRQuantTab) %>% 
    body_add_par("", style="Normal") %>%
    body_add_par("Frequency of Tracts by Quartile", style="Normal") %>%
    body_add_flextable(value = TRQuantFREQ) %>%
    body_add_par("", style="Normal") %>%
    body_add_par("Percentage of Tracts by Quartile", style="heading 3") %>%
    body_add_par("", style="Normal") %>%
    body_add_par("Percentage of Tracts by Quartile: Hand Delivery Letter (Internet First)", style="Normal") %>%
    body_add_flextable(value = TRHDLQTAB) %>% 
    body_add_par("", style="Normal") %>%
    body_add_par("Percentage of Tracts by Quartile: Hand Delivery Package (Internet Choice)", style="Normal") %>%
    body_add_flextable(value = TRHDPQTAB) %>% 
    body_add_par("", style="Normal") %>%
    body_add_par("Percentage of Tracts by Quartile: Update/Leave", style="Normal") %>%
    body_add_flextable(value = TRULQTAB) %>% 
    body_end_section_portrait() %>%
    body_add_par("Response Rate Trend by Contact Type", style="Normal") %>%
    body_add_par("", style="Normal") %>%
    body_add_gg(value=TrendAvgLine, width = 8, height = 5, res = 300) %>%
    body_end_section_landscape() %>%
    body_add_par("Response Rate for Contact Types", style="heading 3") %>%
    body_add_par("", style="Normal") %>%
    body_add_par("Home Delivery Letter (Internet First)", style="Normal") %>%
    body_add_par("", style="Normal") %>%
    body_add_gg(value=ALLHDLplot, width = 6, height = 2.5, res = 300) %>%
    body_add_par("Home Delivery Package (Internet Choice)", style="Normal") %>%
    body_add_par("", style="Normal") %>%
    body_add_gg(value=ALLHDPplot, width = 6, height = 2.5, res = 300) %>%
    body_add_par("", style="Normal") %>%
    body_add_par("Update/Leave", style="Normal") %>%
    body_add_par("", style="Normal") %>%
    body_add_gg(value=ALLULplot, width = 6, height = 2.5, res = 300)
  
  return(reportDoc)
}
