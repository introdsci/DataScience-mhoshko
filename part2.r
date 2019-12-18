## ----setup2, echo=FALSE, message=FALSE, error=FALSE, warning=FALSE, results='hide'----
pkgs = c("stringr", "dplyr", "tidyverse", "readr", "ggplot2", "lubridate", "geosphere", "caret", "knitr", "measurements", "data.table", "purrr", "rvest", "curl")
for (pkg in pkgs){
  if (!require(pkg, character.only = T)){
    install.packages(pkg)
    library(pkg)
  }
}

purl("deliverable1.Rmd", output="part1.r")
source("part1.r")

## ----df2-----------------------------------------------------------------
(df)

## ----location2, warning=FALSE--------------------------------------------
df$latDeath <- df$DeathCityCoordinates %>%
  str_extract('\\d\\d\\.\\d+') %>%
  as.double()
df$longDeath <- df$DeathCityCoordinates %>%
  str_extract('\\-*\\d\\d\\.\\d+\\)') %>%
  str_extract('\\-*\\d\\d\\.\\d+') %>%
  as.double()

df$latInjury <- df$InjuryCityCoordinates %>%
  str_extract('\\d\\d\\.\\d+')%>%
  as.double()
df$longInjury <- df$InjuryCityCoordinates %>%
  str_extract('\\-*\\d\\d\\.\\d+\\)') %>%
  str_extract('\\-*\\d\\d\\.\\d+') %>%
  as.double()

df$latResidence <- df$ResidenceCityCoordinates %>%
  str_extract('\\d\\d\\.\\d+')%>%
  as.double()
df$longResidence <- df$ResidenceCityCoordinates %>%
  str_extract('\\-*\\d\\d\\.\\d+\\)') %>%
  str_extract('\\-*\\d\\d\\.\\d+') %>%
  as.double()


df$locationDistance <- distHaversine(cbind(df$longDeath, df$latDeath), cbind(df$longInjury, df$latInjury))
df$locationDistance <- conv_unit(df$locationDistance, "m", "mi")


df$latDeath <- round(df$latDeath, digits=2)
df$longDeath <- round(df$longDeath, digits=2)
df$latInjury <- round(df$latInjury, digits=2)
df$longInjury <- round(df$longInjury, digits=2)
df$latResidence <- round(df$latResidence, digits=2)
df$longResidence <- round(df$longResidence, digits=2)

df$ResidenceCity <- gsub("FALLS VILLAGE", "FALLS", df$ResidenceCity)

## ----webscrape2, warning=FALSE-------------------------------------------
url = "https://www.mapsofworld.com/usa/states/connecticut/lat-long.html"
download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
web <- read_html("scrapedpage.html")

# web = read_html("https://www.mapsofworld.com/usa/states/connecticut/lat-long.html")
my_paragraphs = html_nodes(web, "tbody")
my_tr = html_nodes(my_paragraphs, "tr")

cityname <- my_tr %>%
  html_nodes("td:first_child") %>%
  html_text()
citylat <- my_tr %>%
  html_nodes("td:nth_child(2)") %>%
  html_text() %>%
  as.double()
citylong <- my_tr %>%
  html_nodes("td:nth_child(3)") %>%
  html_text() %>%
  as.double


citycoordinates <- cbind.data.frame(cityname=cityname, lat=citylat, long=citylong) 
citycoordinates <- citycoordinates[1:142,]

citycoordinates$cityname <- citycoordinates$cityname %>%
  toupper() 


citycoordinates$cityname <-  gsub(" CITY", "", citycoordinates$cityname) 
citycoordinates$cityname <- gsub(" BOROUGH", "", citycoordinates$cityname)
citycoordinates$cityname <- gsub("JEWETT", "JEWETT CITY", citycoordinates$cityname)
citycoordinates$cityname <- gsub(" VILLAGE", "", citycoordinates$cityname)
citycoordinates$cityname <- gsub(" \\(BALANCE\\)", "", citycoordinates$cityname)
citycoordinates$cityname <- gsub("MILFORD ", "MILFORD", citycoordinates$cityname)

## ----countyinfo2---------------------------------------------------------
countyinfo <- "uscities.csv" %>%
  read.csv() %>%
  filter(state_name == "Connecticut") %>%
  select(c("city", "county_name", "state_name"))


#fix column names and city names to help join datasets
countyinfo$city <- toupper(countyinfo$city)
countyinfo$county_name <- toupper(countyinfo$county_name)


countyinfo$state_name <- as.character.factor(countyinfo$state_name)
countyinfo$state_name <- "CT"

colnames(countyinfo)[colnames(countyinfo)=="city"]<-"cityname"
countyinfo$cityname <-  gsub(" VILLAGE", "", countyinfo$cityname)
countyinfo$cityname <-  gsub("GLENVILLE", "GREENWICH", countyinfo$cityname)
countyinfo$cityname <-  gsub("MILFORD CITY", "MILFORD", countyinfo$cityname) 
cit <- merge(x = countyinfo, y = citycoordinates, by = "cityname", all.x = TRUE)

#Add two missing row values
cit$lat <- ifelse(cit$cityname=="MILFORD ", 41.23,   cit$lat)
cit$long <- ifelse(cit$cityname=="MILFORD ", -73.06,   cit$long)
cit$lat <- ifelse(cit$cityname=="PLANTSVILLE", 41.58,   cit$lat)
cit$long <- ifelse(cit$cityname=="PLANTSVILLE", -72.89,   cit$long)

## ----join2---------------------------------------------------------------
setDT(df)
setDT(cit)
df[, `:=`(lat.join = latDeath, long.join = longDeath)]
cit[, `:=`(lat.join = lat, long.join = long)]
df <- cit[df, on = c("lat.join", "long.join"),roll = "nearest"]


sum(is.na(df$DeathCity))
sum(is.na(df$DeathCounty))
df$DeathCity <- ifelse(is.na(df$DeathCity), df$cityname,  df$DeathCity)
df$DeathCounty <- ifelse(is.na(df$DeathCounty), df$county_name,  df$DeathCounty)
sum(is.na(df$DeathCity))
sum(is.na(df$DeathCounty))


## ----filter2-------------------------------------------------------------
park <- filter(df, df$cityname=="CONNING TOWERS NAUTILUS PARK")

## ----remove2-------------------------------------------------------------
df$lat <- NULL
df$long <- NULL
df$cityname <- NULL
df$lat.join <- NULL
df$long.join <- NULL
df$state_name <- NULL
df$county_name <- NULL


df[, `:=`(lat.join = latResidence, long.join = longResidence)]
df <- cit[df, on = c("lat.join", "long.join"),roll = "nearest"]


df$ResidenceCity <- ifelse(is.na(df$ResidenceCity), df$cityname,  df$ResidenceCity)
df$ResidenceCounty <- ifelse((is.na(df$ResidenceCounty)), df$county_name,  df$ResidenceCounty)
df$ResidenceState <- ifelse((is.na(df$ResidenceState)), df$state_name,  df$ResidenceState)


df$lat <- NULL
df$long <- NULL
df$cityname <- NULL
df$lat.join <- NULL
df$long.join <- NULL
df$state_name <- NULL
df$county_name <- NULL


df[, `:=`(lat.join = latInjury, long.join = longInjury)]
df <- cit[df, on = c("lat.join", "long.join"),roll = "nearest" ]



df$InjuryCity <- ifelse(is.na(df$InjuryCity), df$cityname,  df$InjuryCity)
df$InjuryCounty <- ifelse(is.na(df$InjuryCounty), df$county_name,  df$InjuryCounty)
df$InjuryState <- ifelse(is.na(df$InjuryState), df$state_name,  df$InjuryState)


## ----unecessary, include=FALSE-------------------------------------------

#potential graph for future:
# filter(df, !is.na(Age)) %>%
#   filter(!is.na(locationDistance)) %>%
#   ggplot(aes(x=locationDistance, y=..count..)) +
#   geom_point(aes(color=Age)) +
#   labs(title='Number of Accidental Drug Related Deaths 2012-2018 in Connecticut by Distance', x='Miles from Death Location', y='Number of Deaths')


#remove no longer needed data
df$lat <- NULL
df$long <- NULL
df$cityname <- NULL
df$lat.join <- NULL
df$long.join <- NULL
df$state_name <- NULL
df$county_name <- NULL
df$latDeath <- NULL
df$longDeath <- NULL
df$latInjury <- NULL
df$longInjury <- NULL
df$latResidence <- NULL
df$longResidence <- NULL

## ----newdf2--------------------------------------------------------------
(df)


## ----model2--------------------------------------------------------------
set.seed(385)
samp <- df %>%
  filter(!is.na(df$locationDistance), !is.na(df$InjuryCity), !is.na(df$Age), 
           !is.na(df$Sex), !is.na(df$numberOfDrugs),  !is.na(df$Location)) 

sample_selection <- samp$locationDistance %>%
  createDataPartition(p=0.75, list = FALSE)
train <- samp[sample_selection, ]
test <- samp[-sample_selection, ]
train_model <- lm(locationDistance ~ Age + Sex + numberOfDrugs + InjuryCity + Location, data=samp)


prediction <- train_model %>% predict(test)
head(prediction)
head(samp$locationDistance)
R2(prediction, test$locationDistance)

## ----plot2---------------------------------------------------------------
ggplot(test, aes(x=prediction, y=locationDistance))+geom_point(color = "blue") +
  labs(title='Distance Between Death and Overdose vs Predicted (v1)', x='Distance From Death Location (miles)', y='Predicted Distance from Death Location (miles)')


## ----secondmodel2--------------------------------------------------------
samp <- df %>%
  filter(!is.na(df$locationDistance), !is.na(df$InjuryCity), !is.na(df$Location)) 

sample_selection <- samp$locationDistance %>%
  createDataPartition(p=0.90, list = FALSE)
train <- samp[sample_selection, ]
test <- samp[-sample_selection, ]
train_model <- lm(locationDistance ~ InjuryCity + Location, data=samp)


prediction <- train_model %>% predict(test)
head(prediction)
head(samp$locationDistance)
R2(prediction, test$locationDistance)


## ----newplot2------------------------------------------------------------
  ggplot(test, aes(x=prediction, y=locationDistance))+geom_point(color = "blue")  +
  labs(title='Distance Between Death and Overdose vs Predicted (v2)', x='Distance From Death Location (miles)', y='Predicted Distance from Death Location (miles)')


## ----anotherplot2--------------------------------------------------------
filter(df, !is.na(locationDistance)) %>%
  filter(!is.na(Location)) %>%
  ggplot(aes(x=locationDistance))+ geom_line(aes(y=..count.., color=Location), stat="bin", binwidth=50)+
  labs(title='Location of Death vs Overdose', x='Distance between Overdose and Death (miles)', y='Number of Deaths')




filter(df, !is.na(Location), !is.na(locationDistance), !is.na(InjuryCounty), InjuryCounty!="WASHINGTON", InjuryCounty!="WESTCHESTER") %>%
  ggplot(aes(x=InjuryCounty, y=locationDistance)) + geom_point(aes(color=Location)) +
  labs(title="Distance Between Overdose and Death by County", x="Overdose County", y="Distance from Death Location (miles)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ----fent2---------------------------------------------------------------
filter(df, !is.na(Year)) %>%
  ggplot(aes(x=Year, y=..count..))+
  geom_bar(aes(fill=Fentanyl)) + 
  labs(title='Overdoses Involving Fentanyl over Time', x='Year', y='Number of Deaths') +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)


ggplot(df, aes(x=Fentanyl, y=..count..))+
  geom_bar(aes(fill=Location)) + 
  labs(title='Overdoses Involving Fentanyl by Location Type', x='Fentanyl Involved', y='Number of Deaths') +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)






## ------------------------------------------------------------------------
filter(df, !is.na(Year)) %>%
  ggplot(aes(x=Year, y=..count..))+
  geom_bar(aes(fill=factor(DeathCounty))) + 
  labs(title='Number of Accidental Drug Related Deaths 2012-2018 in Connecticut by Year', x='Year', y='Number of Deaths') +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)



filter(df, !is.na(DeathCounty)) %>% 
  ggplot(aes(x=DeathCounty)) +
  geom_bar(stat="count") + 
  labs(title='Number of Accidental Drug Related Deaths 2012-2018 in Connecticut by County', x='County', y='Number of Deaths') + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)


