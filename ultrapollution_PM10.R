# This script performs the econometric analyses for the ultrapollution project

# Loading libraries

library(stargazer)
library(sandwich)
library(Matching)
library(kableExtra)

# Specifying directories for data + results

direc <- paste('C:/Users/', username, '/Documents/Data/ultrapollution/ultra_data/', sep = '')
direc2 <- paste('C:/Users/', username, '/Documents/Data/ultrapollution/results/', sep = '')

# Reading in the data set

data <- read.csv(paste(direc, 'ultradata.csv', sep = ''))

# See which event types have at least 100 observations

event_types <- c()
for (d in unique(data$RACE_Distance)) {if (dim(data[which(data$RACE_Distance == d),][1])>100) {event_types <- c(event_types,d)}}

# Remove ambiguous event types and sub-ultras from list

event_types <- event_types[! event_types %in% c('Variable Hours', 'Unknown Miles', '25 KM', '20 Miles', '40 KM')]

# Remove event types with fewer than 5 unique events and 20 total times events were held and at least 500 observations

drop_types <- c()

for (d in event_types) {
  
  tmp <- data[which(data$RACE_Distance == d),]
  v1 <- length(unique(tmp$RACE_Name))
  v2 <- length(unique(tmp$RACE_ID))
  v3 <- dim(tmp)[1]
  
  if (v1 < 5 || v2 < 20 || v3 < 500) {
    
    drop_types <- c(drop_types,d)
    
  }
  
}

event_types <- event_types[! event_types %in% drop_types]

# Split event_types into distance based and time based events lists

event_types_db <- event_types[c(1,4,6,11,12,13,15,16,17,18,19,20,21,22,23,25,26,27,28)]
event_types_tb <- event_types[c(2,3,5,7,8,9,10,14,24)]

"I NEED TO GET TOTAL NUMBEROF EVENT TYPES, WHAT IS KEPT, WHAT IS NOT, ALONG WITH RELEVANT SUMMARY STATS ON THESE"

# Run OLS regressions for distance based events

modd1 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[1]),])

cov <- vcovHC(modd1, type = 'HC0')
hrsed1 <- sqrt(diag(cov))

modd2 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[2]),])

cov <- vcovHC(modd2, type = 'HC0')
hrsed2 <- sqrt(diag(cov))

modd3 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[3]),])

cov <- vcovHC(modd3, type = 'HC0')
hrsed3 <- sqrt(diag(cov))

modd4 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[4]),])

cov <- vcovHC(modd4, type = 'HC0')
hrsed4 <- sqrt(diag(cov))

modd5 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[5]),])

cov <- vcovHC(modd5, type = 'HC0')
hrsed5 <- sqrt(diag(cov))

modd6 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[6]),])

cov <- vcovHC(modd6, type = 'HC0')
hrsed6 <- sqrt(diag(cov))

modd7 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[7]),])

cov <- vcovHC(modd7, type = 'HC0')
hrsed7 <- sqrt(diag(cov))

modd8 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[8]),])

cov <- vcovHC(modd8, type = 'HC0')
hrsed8 <- sqrt(diag(cov))

modd9 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[9]),])

cov <- vcovHC(modd9, type = 'HC0')
hrsed9 <- sqrt(diag(cov))

modd10 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[10]),])

cov <- vcovHC(modd10, type = 'HC0')
hrsed10 <- sqrt(diag(cov))

modd11 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[11]),])

cov <- vcovHC(modd11, type = 'HC0')
hrsed11 <- sqrt(diag(cov))

modd12 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[12]),])

cov <- vcovHC(modd12, type = 'HC0')
hrsed12 <- sqrt(diag(cov))

modd13 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[13]),])

cov <- vcovHC(modd13, type = 'HC0')
hrsed13 <- sqrt(diag(cov))

modd14 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[14]),])

cov <- vcovHC(modd14, type = 'HC0')
hrsed14 <- sqrt(diag(cov))

modd15 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[15]),])

cov <- vcovHC(modd15, type = 'HC0')
hrsed15 <- sqrt(diag(cov))

modd16 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[16]),])

cov <- vcovHC(modd16, type = 'HC0')
hrsed16 <- sqrt(diag(cov))

modd17 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[17]),])

cov <- vcovHC(modd17, type = 'HC0')
hrsed17 <- sqrt(diag(cov))

modd18 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[18]),])

cov <- vcovHC(modd18, type = 'HC0')
hrsed18 <- sqrt(diag(cov))

modd19 <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
             + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_db[19]),])

cov <- vcovHC(modd19, type = 'HC0')
hrsed19 <- sqrt(diag(cov))

# Run OLS regressions for time based events

modt1 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[1]),])

cov <- vcovHC(modt1, type = 'HC0')
hrset1 <- sqrt(diag(cov))

modt2 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[2]),])

cov <- vcovHC(modt2, type = 'HC0')
hrset2 <- sqrt(diag(cov))

modt3 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[3]),])

cov <- vcovHC(modt3, type = 'HC0')
hrset3 <- sqrt(diag(cov))

modt4 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[4]),])

cov <- vcovHC(modt4, type = 'HC0')
hrset4 <- sqrt(diag(cov))

modt5 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[5]),])

cov <- vcovHC(modt5, type = 'HC0')
hrset5 <- sqrt(diag(cov))

modt6 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[6]),])

cov <- vcovHC(modt6, type = 'HC0')
hrset6 <- sqrt(diag(cov))

modt7 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[7]),])

cov <- vcovHC(modt7, type = 'HC0')
hrset7 <- sqrt(diag(cov))

modt8 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[8]),])

cov <- vcovHC(modt8, type = 'HC0')
hrset8 <- sqrt(diag(cov))

modt9 <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
            + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State + Travel_Distance
            + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance == event_types_tb[9]),])

cov <- vcovHC(modt9, type = 'HC0')
hrset9 <- sqrt(diag(cov))

# Viewing results

write.csv(stargazer(modd1, modd2, modd3, modd4, modd5, modd6, modd7, modd8, modd9, modd10,
                    modd11, modd12, modd13, modd14, modd15, modd16, modd17, modd18, modd19,
                    se = list(hrsed1, hrsed2, hrsed3, hrsed4, hrsed5, hrsed6, hrsed7, hrsed8, hrsed9, hrsed10,
                              hrsed11, hrsed12, hrsed13, hrsed14, hrsed15, hrsed16, hrsed17, hrsed18, hrsed19),
                    omit = c('FIPS_Race', 'RACE_Name', 'RACE_Month', 'RACE_Year'),
                    omit.stat = c('f', 'ser'), type = 'text'), 
          paste(direc2, 'PM10_db.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(modt1, modt2, modt3, modt4, modt5, modt6, modt7, modt8, modt9,
                    se = list(hrset1, hrset2, hrset3, hrset4, hrset5, hrset6, hrset7, hrset8, hrset9),
                    omit = c('FIPS_Race', 'RACE_Name', 'RACE_Month', 'RACE_Year'),
                    omit.stat = c('f', 'ser'), type = 'text'),
          paste(direc2, 'PM10_tb.txt', sep = ''), row.names = FALSE)


"DO A SCRIPT FOR EACH POLLUTANT + ANOTHER SCRIPT FOR AN AGGREGATE POLLUTION SCORE?"
"IF AGGREGATE SCORE IS USED :: WHAT IS THE BEST WAY TO AGGREGATE? LOOK AT HISTS"


