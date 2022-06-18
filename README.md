
ibrary(plyr)
library(rvest)
library(broom)
library(tidyverse)
library(geosphere)
library(ggmap)
library(RDSTK)
library(httr)
library(rjson)
library(jsonlite)
library(tibble)
library(sf)
library(mapview)


#establishing rounds and years
rounds <- c(1:20)
year <- c(2009:2021)
#using baseball reference url
#Storing lists inside a list, urls inside years_urls
urls <- list()
years_urls <- list()
for (k in 1:13){
bref_begin <- paste0('https://www.baseball-reference.com/draft/?year_ID=' , year[k], '&draft_round=')
  for (i in 1: 20){
  url <- paste0(bref_begin, rounds[i], '&draft_type=junreg&query_type=year_round&from_type_jc=0&from_type_hs=0&from_type_4y=0&from_type_unk=0')
  urls[[i]] <- url
  }
years_urls[[k]] <- urls
}

table_holder <- list() 
j <- 1
l <- 1
m <- 1
#accessing main list, and running a nested for loop to turn htmls from nested list into tables and storing into larger list
for (l in seq_along(years_urls)){
list_holder <- years_urls[[l]]
  for (j in seq_along(list_holder)) {
  table_holder[[m]] <- list_holder [[j]] %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  j <- j + 1
  m <- m + 1
  }
l <- l + 1
}

#This will create a dataframe containing the draft data of rounds 1:20 from 2009 to 2021
xsm_bref13draft <- ldply(table_holder, data.frame)
xsm_bref13draft

#this code will clean the data

#converts the rounds into an integer
for (i in 1:nrow(xsm_bref13draft)){
  if(grepl("s", xsm_bref13draft$Rnd[i]) == TRUE){
    xsm_bref13draft$Rnd[i] <- gsub("s", '', xsm_bref13draft$Rnd[i])
  }
}
xsm_bref13draft$Rnd <- as.integer(xsm_bref13draft$Rnd)

#converting the signed or not into a dummy variable
xsm_bref13draft$Signed <- ifelse(xsm_bref13draft$Signed == "Y", 1, 0)

#removing players that did not sign, this way I avoid repeats
#There is the scenario where a player that did not sign was not taken rounds 1-20 in there next draft
#Although not a significant number would follow the scenario above, if it needs to be taken in account, do not run
#the next line of code
xsm_bref13draft <- xsm_bref13draft %>% filter(Signed == 1)


#removes dollar sign from bonus
xsm_bref13draft <- apply(xsm_bref13draft, 2, function(Bonus) gsub("\\$", "", Bonus))
xsm_bref13draft <- as.data.frame(xsm_bref13draft)
#removing commas from bonus
xsm_bref13draft$Bonus <- as.numeric(gsub(",","",xsm_bref13draft$Bonus))
#converting necessary characters to integers, since it altered the dataframe
xsm_bref13draft$Bonus <- as.numeric(xsm_bref13draft$Bonus)
xsm_bref13draft$Rnd <- as.numeric(xsm_bref13draft$Rnd)
xsm_bref13draft$OvPck <- as.numeric(xsm_bref13draft$OvPck)
xsm_bref13draft$RdPck <- as.numeric(xsm_bref13draft$RdPck)
xsm_bref13draft$Signed <- as.numeric(xsm_bref13draft$Signed)
xsm_bref13draft$G <- as.numeric(xsm_bref13draft$G)
xsm_bref13draft$G.1 <- as.numeric(xsm_bref13draft$G.1)

#converting the year to numeric (not really necessary but just in case)
xsm_bref13draft$Year <- as.numeric(xsm_bref13draft$Year)

sum(is.na(xsm_bref13draft$G))
#removing (minors) from name
xsm_bref13draft$Name <- gsub("\\s*\\([^\\)]+\\)","",as.character(xsm_bref13draft$Name))

#extra dataframe
xsm_bref13draft2 <- xsm_bref13draft

#splitting teams to just include who drafted the player for simplicity
for(i in 1:nrow(xsm_bref13draft)){
  n1 <- unlist(strsplit(xsm_bref13draft$Tm[[i]], split = 'via', fixed = TRUE))[1]
  n1 <- trimws(n1)
  xsm_bref13draft$Tm[[i]] <- n1
}


#creating another column of positions that are consolidated
xsm_bref13draft['Position_consolidate'] <- NA

#need to turn the bt into capital letters
xsm_bref13draft$B.T <- toupper(xsm_bref13draft$B.T)

#consolidates positions
for (i in 1: nrow(xsm_bref13draft)){
  if(xsm_bref13draft$Pos[[i]] == "1B" |xsm_bref13draft$Pos[[i]] == "3B" ){
    xsm_bref13draft$Position_consolidate[[i]] <- "CI"
  }
  if(xsm_bref13draft$Pos[[i]] == "2B" |xsm_bref13draft$Pos[[i]] == "SS" | xsm_bref13draft$Pos[[i]] == "UT" | xsm_bref13draft$Pos[[i]] == "IF"){
    xsm_bref13draft$Position_consolidate[[i]] <- "MI"
  }
    if(xsm_bref13draft$Pos[[i]] == "C" |xsm_bref13draft$Pos[[i]] == "c"){
    xsm_bref13draft$Position_consolidate[[i]] <- "C"
    }
    if(xsm_bref13draft$Pos[[i]] == "CF" |xsm_bref13draft$Pos[[i]] == "LF"|xsm_bref13draft$Pos[[i]] == "RF" | xsm_bref13draft$Pos[[i]] == "OF" ){
    xsm_bref13draft$Position_consolidate[[i]] <- "OF"
    }
    if(xsm_bref13draft$Pos[[i]] == "RHp" | xsm_bref13draft$Pos[[i]] == "RHP"){
      xsm_bref13draft$Position_consolidate[[i]] <- "RHP"
    }
   if(xsm_bref13draft$Pos[[i]] == "LHp" | xsm_bref13draft$Pos[[i]] == "LHP"){
      xsm_bref13draft$Position_consolidate[[i]] <- "LHP"
   }
  
  if (xsm_bref13draft$Pos[[i]] == "P"){
    if (grepl("/R", xsm_bref13draft$B.T[[i]]) == TRUE){
      xsm_bref13draft$Position_consolidate[[i]] <- "RHP"
    }
    else{
      xsm_bref13draft$Position_consolidate[[i]] <- "LHP"
    }
  }
}

xsm_bref13draft['Majors'] <- NA

xsm_bref13draft$Majors <- ifelse(is.na(xsm_bref13draft$G) & is.na(xsm_bref13draft$G.1), 0, 1)



#longitude and latitude for all 30 mlb team stadiums

ARI_lon <- -112.066664
ARI_lat <- 33.445526

ATL_lon <- -84.467684
ATL_lat <- 33.8907

BAL_lon <- -76.622368
BAL_lat <- 39.284176

BOS_lon <- -71.095764
BOS_lat <- 42.346268
  
CHC_lon <- -87.655800
CHC_lat <- 	41.948463
  
CWS_lon <- -87.634598
CWS_lat <- 41.830017
  
CIN_lon <- -84.507103
CIN_lat <- 39.097458
  
CLE_lon <- -81.686043
CLE_lat <- 	41.496262
  
COL_lon <- -104.994865
COL_lat <- 	39.756229

DET_lon <- 	-83.048134
DET_lat <- 	42.338356
  
HOU_lon <- -95.356209
HOU_lat <- 29.757017
  
KC_lon <- -94.480682
KC_lat <- 39.051910
  
LAA_lon <- -117.883438
LAA_lat <- 33.800560
  
LAD_lon <- -118.240784
LAD_lat <- 34.073814
  
MIA_lon <- 	-80.220352
MIA_lat <- 	25.778301

MIL_lon <- 	-87.971497
MIL_lat <- 	43.027954

MIN_lon <- 	-93.278435
MIN_lat <- 	44.982075

NYM_lon <- 	-73.846237
NYM_lat <- 	40.757256
  
NYY_lon <- -73.926186
NYY_lat <- 	40.829659
  
OAK_lon <- -122.201553
OAK_lat <- 37.751637
  
PHI_lon <- 	-75.167465
PHI_lat <- 	39.906216
  
PIT_lon <- -80.006363
PIT_lat <- 	40.447105

SD_lon <- 	-117.157516
SD_lat <- 	32.707535

SF_lon <- -122.389717
SF_lat <- 	37.778572
  
SEA_lon <- -122.332863
SEA_lat <- 47.591480

STL_lon <- -90.193329
STL_lat <- 	38.622780

TB_lon <- -82.653961
TB_lat <- 27.768284
  
TEX_lon <- -97.0830
TEX_lat <- 32.7510
  
TOR_lon <- 	-79.390083
TOR_lat <- 	43.641796

WAS_lon <- 	-77.007996
WAS_lat <- 	38.873055

#Paste key here, get your own key
register_google(key = $$$$$$$$$$$$$$$)



# Creating loop that will create dataframe
playercord <- data.frame(matrix(nrow = nrow(xsm_bref13draft), ncol = 2))
for(i in 1:nrow(xsm_bref13draft)){
  playercord [i,] <- geocode(xsm_bref13draft$Drafted.Out.of[[i]])
}

names(playercord) <- c("Longitude", "Latitude")

#adding this to main dataframe

xsm_bref13draft <- cbind(xsm_bref13draft, playercord)

xsm_bref13draft['Distance'] <- NA


#creating 30 if statements to find distance from drafted team
for (i in 1:nrow(xsm_bref13draft)){
  if(xsm_bref13draft[i,7] == "Angels"){
    xsm_bref13draft$Distance[[i]] <- distm(c(LAA_lon, LAA_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Astros"){
    xsm_bref13draft$Distance[[i]] <- distm(c(HOU_lon, HOU_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Athletics"){
    xsm_bref13draft$Distance[[i]] <- distm(c(OAK_lon, OAK_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Blue Jays"){
    xsm_bref13draft$Distance[[i]] <- distm(c(TOR_lon, TOR_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Braves"){
    xsm_bref13draft$Distance[[i]] <- distm(c(ATL_lon, ATL_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Brewers"){
   xsm_bref13draft$Distance[[i]] <- distm(c(MIL_lon, MIL_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Cardinals"){
    xsm_bref13draft$Distance[[i]] <- distm(c(STL_lon, STL_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Cubs"){
    xsm_bref13draft$Distance[[i]] <- distm(c(CHC_lon, CHC_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7]== "Diamondbacks"){
   xsm_bref13draft$Distance[[i]] <- distm(c(ARI_lon, ARI_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Dodgers"){
    xsm_bref13draft$Distance[[i]] <- distm(c(LAD_lon, LAD_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Giants"){
    xsm_bref13draft$Distance[[i]] <- distm(c(SF_lon, SF_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Indians"){
    xsm_bref13draft$Distance[[i]] <- distm(c(CLE_lon, CLE_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Mariners"){
    xsm_bref13draft$Distance[[i]] <- distm(c(SEA_lon, SEA_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Marlins"){
    xsm_bref13draft$Distance[[i]] <- distm(c(MIA_lon, MIA_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Mets"){
    xsm_bref13draft$Distance[[i]] <- distm(c(NYM_lon, NYM_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Nationals"){
    xsm_bref13draft$Distance[[i]] <- distm(c(WAS_lon, WAS_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Orioles"){
   xsm_bref13draft$Distance[[i]] <- distm(c(BAL_lon, BAL_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Padres"){
    xsm_bref13draft$Distance[[i]] <- distm(c(SD_lon, SD_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Phillies"){
    xsm_bref13draft$Distance[[i]] <- distm(c(PHI_lon, PHI_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Pirates"){
   xsm_bref13draft$Distance[[i]] <- distm(c(PIT_lon, PIT_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Rangers"){
    xsm_bref13draft$Distance[[i]] <- distm(c(TEX_lon, TEX_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Rays"){
    xsm_bref13draft$Distance[[i]] <- distm(c(TB_lon, TB_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Red Sox"){
    xsm_bref13draft$Distance[[i]] <- distm(c(BOS_lon, BOS_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Reds"){
    xsm_bref13draft$Distance[[i]] <- distm(c(CIN_lon, CIN_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Rockies"){
    xsm_bref13draft$Distance[[i]] <- distm(c(COL_lon, COL_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Royals"){
    xsm_bref13draft$Distance[[i]] <- distm(c(KC_lon, KC_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Tigers"){
    xsm_bref13draft$Distance[[i]] <- distm(c(DET_lon, DET_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Twins"){
    xsm_bref13draft$Distance[[i]] <- distm(c(MIN_lon, MIN_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "White Sox"){
   xsm_bref13draft$Distance[[i]] <- distm(c(CWS_lon, CWS_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }
  if(xsm_bref13draft[i,7] == "Yankees"){
   xsm_bref13draft$Distance[[i]] <- distm(c(NYY_lon, NYY_lat), c(xsm_bref13draft$Longitude[[i]], xsm_bref13draft$Latitude[[i]]), fun = distHaversine)
  }

}


#converting meters to miles
for (i in 1: 1562){
  xsm_bref13draft$Distance[[i]] <- (xsm_bref13draft$Distance[[i]])/1609.344
}

