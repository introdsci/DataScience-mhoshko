## ---- echo=FALSE, message=FALSE, error=FALSE, warning=FALSE, results='hide'----
pkgs = c("stringr", "dplyr", "tidyverse", "readr", "ggplot2", "lubridate")
for (pkg in pkgs){
  if (!require(pkg, character.only = T)){
    install.packages(pkg)
    library(pkg)
  }
}


## ------------------------------------------------------------------------
df <- read.csv('Drug Death Data.csv', na.strings=c("","NA"))
head(df)
df <- select(df, -"ID")

df$Date <- gsub( " .*$", "", df$Date )
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
df$Year <- year(df$Date)
df$Month <- month(df$Date)
df$MonthandYear <- format(df$Date, "%m/%Y")

df$numberOfDrugs <- 0

## ------------------------------------------------------------------------
colnames(df)[colnames(df)=="COD"] <- "CauseOfDeath"
colnames(df)[colnames(df)=="DeathCityGeo"]<-"DeathCityCoordinates"
colnames(df)[colnames(df)=="ResidenceCityGeo"]<-"ResidenceCityCoordinates"
colnames(df)[colnames(df)=="InjuryCityGeo"]<-"InjuryCityCoordinates"
df$DeathCityCoordinates <- gsub(".*\\n", "", df$DeathCityCoordinates)
df$ResidenceCityCoordinates <- gsub(".*\\n", "", df$ResidenceCityCoordinates)
df$InjuryCityCoordinates <- gsub(".*\\n", "", df$InjuryCityCoordinates)


#Setting columns to factors
df$Location <- as.factor(df$Location)
df$MannerofDeath <- tolower(df$MannerofDeath)
df$MannerofDeath <- factor(df$MannerofDeath)
df$DeathCity <- as.character.factor(df$DeathCity)
df$InjuryCity <- as.character.factor(df$InjuryCity)
df$ResidenceCity <- as.character.factor(df$ResidenceCity)
df$DeathCounty <- as.character.factor(df$DeathCounty)
df$InjuryCounty <- as.character.factor(df$InjuryCounty)
df$ResidenceCounty <- as.character.factor(df$ResidenceCounty)
df$InjuryState <- as.character.factor(df$InjuryState)
df$ResidenceState <- as.character.factor(df$ResidenceState)
df$DeathCounty[df$DeathCounty=="USA"] <- NA

## ------------------------------------------------------------------------

df$numberOfDrugs <- ifelse(is.na(df$OtherSignifican), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$OtherSignifican <- ifelse(is.na(df$OtherSignifican), "N", "Y")

## ---- include=FALSE------------------------------------------------------
df$numberOfDrugs <- ifelse(is.na(df$Heroin), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Heroin <- ifelse(is.na(df$Heroin), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Cocaine), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Cocaine <- ifelse(is.na(df$Cocaine), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Fentanyl), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Fentanyl <- ifelse(is.na(df$Fentanyl), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$FentanylAnalogue), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$FentanylAnalogue <- ifelse(is.na(df$FentanylAnalogue), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Oxycodone), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Oxycodone <- ifelse(is.na(df$Oxycodone), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Oxymorphone), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Oxymorphone <- ifelse(is.na(df$Oxymorphone), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Ethanol), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Ethanol <- ifelse(is.na(df$Ethanol), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Hydrocodone), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Hydrocodone <- ifelse(is.na(df$Hydrocodone), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Benzodiazepine), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Benzodiazepine <- ifelse(is.na(df$Benzodiazepine), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Methadone), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Methadone <- ifelse(is.na(df$Methadone), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Amphet), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Amphet <- ifelse(is.na(df$Amphet), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Tramad), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Tramad <- ifelse(is.na(df$Tramad), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Morphine_NotHeroin), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Morphine_NotHeroin <- ifelse(is.na(df$Morphine_NotHeroin), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Hydromorphone), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Hydromorphone <- ifelse(is.na(df$Hydromorphone), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$Other), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$Other <- ifelse(is.na(df$Other), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$AnyOpioid), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$AnyOpioid <- ifelse(is.na(df$AnyOpioid), "N", "Y")
df$numberOfDrugs <- ifelse(is.na(df$OpiateNOS), df$numberOfDrugs+0, df$numberOfDrugs+1)
df$OpiateNOS <- ifelse(is.na(df$OpiateNOS), "N", "Y")


## ------------------------------------------------------------------------
head(df)

## ------------------------------------------------------------------------
filter(df, !is.na(Age)) %>%
  ggplot(aes(x=Age)) + 
  geom_histogram(binwidth = .5) + 
  labs(title='Ages of Accidental Drug Related Deaths 2012-2018 in Connecticut', x='Age', y='Number of Deaths')


filter(df, !is.na(DeathCounty)) %>% 
  ggplot(aes(x=DeathCounty)) +
  geom_bar(stat="count") + 
  labs(title='Number of Accidental Drug Related Deaths 2012-2018 in Connecticut by County', x='County', y='Number of Deaths') + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)

## ------------------------------------------------------------------------
filter(df, !is.na(Year)) %>%
  ggplot(aes(x=Year, y=..count..))+
  geom_bar(aes(fill=factor(DeathCounty))) + 
  labs(title='Number of Accidental Drug Related Deaths 2012-2018 in Connecticut by Year', x='Year', y='Number of Deaths') +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.25)


df %>%
   filter(!is.na(Fentanyl)) %>%
   filter(!is.na(Year))%>%
   group_by(Year) %>%
   summarise(Sum_of_Fentanyl_Deaths=n())

df %>%
  filter(Fentanyl=="Y") %>%
  filter(!is.na(Year)) %>%
  ggplot(aes(x=Date))+ geom_line(aes(y=..count..), stat="bin", binwidth=35)+
  labs(title="Deaths involving Fentanyl", x="Year", y="Number of Deaths") 


df %>%
   filter(!is.na(Fentanyl)) %>%
   filter(!is.na(Year))%>%
   group_by(Year) %>%
   summarise(Sum_of_Non_Fentanyl_Deaths=n())

df %>%
  filter(Fentanyl=="N") %>%
  filter(!is.na(Year)) %>%  
  ggplot(aes(x=Date))+ geom_line(aes(y=..count..), stat="bin", binwidth=25) +
  labs(title="Deaths Not involving Fentanyl", x="Date", y="Number of Deaths")
  

df %>%
   filter(!is.na(Age)) %>%
   filter(!is.na(Year)) %>%
   group_by(Year) %>%
   summarise(Mean_Age = mean(Age))

df %>%
  filter(!is.na(numberOfDrugs)) %>%
  filter(!is.na(Year)) %>%
  group_by(Year) %>%
  summarise(Mean_Number_of_Drugs = mean(numberOfDrugs))

df %>%
  filter(!is.na(numberOfDrugs)) %>%
  filter(!is.na(Year)) %>%
  filter(!is.na(Sex)) %>%
  group_by(Year) %>%
  group_by(Sex) %>%
  summarise(num_deaths = n())


filter(df, !is.na(Date)) %>%
  filter(!is.na(Sex)) %>%
  ggplot(aes(x=Date)) + geom_line(aes(y=..count.., color=Sex), stat="bin", binwidth=50) +
  labs(title="Number of Deaths over Time by Gender", x="Date", y="Number of Deaths")


