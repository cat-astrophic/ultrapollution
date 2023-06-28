# This script performs the econometric analyses for the ultrapollution project

# Loading libraries

library(stargazer)
library(sandwich)
library(ggplot2)
library(jtools)
library(dplyr)
library(kableExtra)
library(modelsummary)
library(maps)
library(sf)
library(tmap)
library(socviz)
library(lmtest)

# Specifying directories for data + results

direc <- paste('C:/Users/', username, '/Documents/Data/ultrapollution/ultra_data/', sep = '')
direc2 <- paste('C:/Users/', username, '/Documents/Data/ultrapollution/results/', sep = '')

# Reading in the data set

data <- read.csv(paste(direc, 'ultradata.csv', sep = ''))

# See which event types have at least 100 observations

event_types <- c()
for (d in unique(data$RACE_Distance)) {if (dim(data[which(data$RACE_Distance == d),])[1]>100) {event_types <- c(event_types,d)}}

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

# Making sure that no negative pollution values exist

data$PM2.5 <- ifelse(data$PM2.5 < 0, 0, data$PM2.5)
data$PM10 <- ifelse(data$PM10 < 0, 0, data$PM10)
data$CO <- ifelse(data$CO < 0, 0, data$CO)
data$NO2 <- ifelse(data$NO2 < 0, 0, data$NO2)
data$O3 <- ifelse(data$O3 < 0, 0, data$O3)

# Making sure that no negative pollution values exist for home counties

data$PM2.5_Home <- ifelse(data$PM2.5_Home < 0, 0, data$PM2.5_Home)
data$PM10_Home <- ifelse(data$PM10_Home < 0, 0, data$PM10_Home)
data$CO_Home <- ifelse(data$CO_Home < 0, 0, data$CO_Home)
data$NO2_Home <- ifelse(data$NO2_Home < 0, 0, data$NO2_Home)
data$O3_Home <- ifelse(data$O3_Home < 0, 0, data$O3_Home)

# Run the main models

modpmd <- lm(log(Seconds+.001) ~ PM2.5 + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modpmdx <- coeftest(modpmd, vcov = vcovCL, cluster = ~RACE_Distance)

modpmt <- lm(log(Distance+.001) ~ PM2.5 + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modpmtx <- coeftest(modpmt, vcov = vcovCL, cluster = ~RACE_Distance)

modpm10d <- lm(log(Seconds+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modpm10dx <- coeftest(modpm10d, vcov = vcovCL, cluster = ~RACE_Distance)

modpm10t <- lm(log(Distance+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modpm10tx <- coeftest(modpm10t, vcov = vcovCL, cluster = ~RACE_Distance)

modcod <- lm(log(Seconds+.001) ~ CO + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modcodx <- coeftest(modcod, vcov = vcovCL, cluster = ~RACE_Distance)

modcot <- lm(log(Distance+.001) ~ CO + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modcotx <- coeftest(modcot, vcov = vcovCL, cluster = ~RACE_Distance)

modno2d <- lm(log(Seconds+.001) ~ NO2 + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modno2dx <- coeftest(modno2d, vcov = vcovCL, cluster = ~RACE_Distance)

modno2t <- lm(log(Distance+.001) ~ NO2 + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modno2tx <- coeftest(modno2t, vcov = vcovCL, cluster = ~RACE_Distance)

modo3d <- lm(log(Seconds+.001) ~ O3 + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modo3dx <- coeftest(modo3d, vcov = vcovCL, cluster = ~RACE_Distance)

modo3t <- lm(log(Distance+.001) ~ O3 + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modo3tx <- coeftest(modo3t, vcov = vcovCL, cluster = ~RACE_Distance)

modalld <- lm(log(Seconds+.001) ~ PM2.5 + factor(Gender) + PM10 + factor(Gender) + CO + factor(Gender) + NO2 + factor(Gender) + O3 + factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate  + Altitude_Home
              + PM2.5_Home + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modalldx <- coeftest(modalld, vcov = vcovCL, cluster = ~RACE_Distance)

modallt <- lm(log(Distance+.001) ~ PM2.5 + factor(Gender) + PM10 + factor(Gender) + CO + factor(Gender) + NO2 + factor(Gender) + O3 + factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM2.5_Home + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modalltx <- coeftest(modallt, vcov = vcovCL, cluster = ~RACE_Distance)

mod41d <- lm(log(Seconds+.001) ~ PM2.5 + factor(Gender) + CO + factor(Gender) + NO2 + factor(Gender) + O3 + factor(Gender)
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home
             + PM2.5_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

mod41dx <- coeftest(mod41d, vcov = vcovCL, cluster = ~RACE_Distance)

mod41t <- lm(log(Distance+.001) ~ PM2.5 + factor(Gender) + CO + factor(Gender) + NO2 + factor(Gender) + O3 + factor(Gender)
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home
             + PM2.5_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

mod41tx <- coeftest(mod41t, vcov = vcovCL, cluster = ~RACE_Distance)

mod42d <- lm(log(Seconds+.001) ~ PM10 + factor(Gender) + CO + factor(Gender) + NO2 + factor(Gender) + O3 + factor(Gender)
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home
             + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

mod42dx <- coeftest(mod42d, vcov = vcovCL, cluster = ~RACE_Distance)

mod42t <- lm(log(Distance+.001) ~ PM10 + factor(Gender) + CO + factor(Gender) + NO2 + factor(Gender) + O3 + factor(Gender)
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home
             + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

mod42tx <- coeftest(mod42t, vcov = vcovCL, cluster = ~RACE_Distance)

# Results

write.csv(stargazer(modpmdx, modpm10dx, modcodx, modno2dx, modo3dx, modalldx, mod41dx, mod42dx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_db.txt', sep = ''))

write.csv(stargazer(modpmtx, modpm10tx, modcotx, modno2tx, modo3tx, modalltx, mod41tx, mod42tx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_tb.txt', sep = ''))

stargazer(modpmdx, modpm10dx, modcodx, modno2dx, modo3dx, modalldx, mod41dx, mod42dx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

stargazer(modpmtx, modpm10tx, modcotx, modno2tx, modo3tx, modalltx, mod41tx, mod42tx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

# Run the elasticity models

modpmd2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modpmdxx <- coeftest(modpmd2, vcov = vcovCL, cluster = ~RACE_Distance)

modpmt2 <- lm(log(Distance+.001) ~ log(PM2.5+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modpmtxx <- coeftest(modpmt2, vcov = vcovCL, cluster = ~RACE_Distance)

modpm10d2 <- lm(log(Seconds+.001) ~ log(PM10+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modpm10dxx <- coeftest(modpm10d2, vcov = vcovCL, cluster = ~RACE_Distance)

modpm10t2 <- lm(log(Distance+.001) ~ log(PM10+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modpm10txx <- coeftest(modpm10t2, vcov = vcovCL, cluster = ~RACE_Distance)

modcod2 <- lm(log(Seconds+.001) ~ log(CO+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modcodxx <- coeftest(modcod2, vcov = vcovCL, cluster = ~RACE_Distance)

modcot2 <- lm(log(Distance+.001) ~ log(CO+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modcotxx <- coeftest(modcot2, vcov = vcovCL, cluster = ~RACE_Distance)

modno2d2 <- lm(log(Seconds+.001) ~ log(NO2+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modno2dxx <- coeftest(modno2d2, vcov = vcovCL, cluster = ~RACE_Distance)

modno2t2 <- lm(log(Distance+.001) ~ log(NO2+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modno2txx <- coeftest(modno2t2, vcov = vcovCL, cluster = ~RACE_Distance)

modo3d2 <- lm(log(Seconds+.001) ~ log(O3+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

modo3dxx <- coeftest(modo3d2, vcov = vcovCL, cluster = ~RACE_Distance)

modo3t2 <- lm(log(Distance+.001) ~ log(O3+.001) + factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

modo3txx <- coeftest(modo3t2, vcov = vcovCL, cluster = ~RACE_Distance)

modalld2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001) + log(PM10+.001) + log(CO+.001) + log(NO2+.001) + log(O3+.001) + factor(Gender)
               + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home
               + PM2.5_Home + PM10_Home + CO_Home + NO2_Home + O3_Home,
               data = data[which(data$RACE_Distance %in% event_types_db),])

modalldxx <- coeftest(modalld2, vcov = vcovCL, cluster = ~RACE_Distance)

modallt2 <- lm(log(Distance+.001) ~ log(PM2.5+.001) + log(PM10+.001) + log(CO+.001) + log(NO2+.001) + log(O3+.001) + factor(Gender)
               + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home
               + PM2.5_Home + PM10_Home + CO_Home + NO2_Home + O3_Home,
               data = data[which(data$RACE_Distance %in% event_types_tb),])

modalltxx <- coeftest(modallt2, vcov = vcovCL, cluster = ~RACE_Distance)

mod41d2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001) + log(CO+.001) + log(NO2+.001) + log(O3+.001) + factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM2.5_Home +CO_Home + NO2_Home + O3_Home,
              data = data[which(data$RACE_Distance %in% event_types_db),])

mod41dxx <- coeftest(mod41d2, vcov = vcovCL, cluster = ~RACE_Distance)

mod41t2 <- lm(log(Distance+.001) ~ log(PM2.5+.001) + log(CO+.001) + log(NO2+.001) + log(O3+.001) + factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM2.5_Home + CO_Home + NO2_Home + O3_Home,
              data = data[which(data$RACE_Distance %in% event_types_tb),])

mod41txx <- coeftest(mod41t2, vcov = vcovCL, cluster = ~RACE_Distance)

mod42d2 <- lm(log(Seconds+.001) ~ log(PM10+.001) + log(CO+.001) + log(NO2+.001) + log(O3+.001) + factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM10_Home + CO_Home + NO2_Home + O3_Home,
              data = data[which(data$RACE_Distance %in% event_types_db),])

mod42dxx <- coeftest(mod42d2, vcov = vcovCL, cluster = ~RACE_Distance)

mod42t2 <- lm(log(Distance+.001) ~ log(PM10+.001) + log(CO+.001) + log(NO2+.001) + log(O3+.001) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM10_Home + CO_Home + NO2_Home + O3_Home,
              data = data[which(data$RACE_Distance %in% event_types_tb),])

mod42txx <- coeftest(mod42t2, vcov = vcovCL, cluster = ~RACE_Distance)

# Results

write.csv(stargazer(modpmdxx, modpm10dxx, modcodxx, modno2dxx, modo3dxx, modalldxx, mod41dxx, mod42dxx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_db_2.txt', sep = ''))

write.csv(stargazer(modpmtxx, modpm10txx, modcotxx, modno2txx, modo3txx, modalltxx, mod41txx, mod42txx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_tb_2.txt', sep = ''))

stargazer(modpmdxx, modpm10dxx, modcodxx, modno2dxx, modo3dxx, modalldxx, mod41dxx, mod42dxx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

stargazer(modpmtxx, modpm10txx, modcotxx, modno2txx, modo3txx, modalltxx, mod41txx, mod42txx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

# Repeating with an interaction between pollution and gender

# Log-level models

xmodpmd <- lm(log(Seconds+.001) ~ PM2.5*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodpmdx <- coeftest(xmodpmd, vcov = vcovCL, cluster = ~RACE_Distance)

xmodpmt <- lm(log(Distance+.001) ~ PM2.5*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodpmtx <- coeftest(xmodpmt, vcov = vcovCL, cluster = ~RACE_Distance)

xmodpm10d <- lm(log(Seconds+.001) ~ PM10*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodpm10dx <- coeftest(xmodpm10d, vcov = vcovCL, cluster = ~RACE_Distance)

xmodpm10t <- lm(log(Distance+.001) ~ PM10*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodpm10tx <- coeftest(xmodpm10t, vcov = vcovCL, cluster = ~RACE_Distance)

xmodcod <- lm(log(Seconds+.001) ~ CO*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodcodx <- coeftest(xmodcod, vcov = vcovCL, cluster = ~RACE_Distance)

xmodcot <- lm(log(Distance+.001) ~ CO*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodcotx <- coeftest(xmodcot, vcov = vcovCL, cluster = ~RACE_Distance)

xmodno2d <- lm(log(Seconds+.001) ~ NO2*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodno2dx <- coeftest(xmodno2d, vcov = vcovCL, cluster = ~RACE_Distance)

xmodno2t <- lm(log(Distance+.001) ~ NO2*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodno2tx <- coeftest(xmodno2t, vcov = vcovCL, cluster = ~RACE_Distance)

xmodo3d <- lm(log(Seconds+.001) ~ O3*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodo3dx <- coeftest(xmodo3d, vcov = vcovCL, cluster = ~RACE_Distance)

xmodo3t <- lm(log(Distance+.001) ~ O3*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodo3tx <- coeftest(xmodo3t, vcov = vcovCL, cluster = ~RACE_Distance)

xmodalld <- lm(log(Seconds+.001) ~ PM2.5*factor(Gender) + PM10*factor(Gender) + CO*factor(Gender) + NO2*factor(Gender) + O3*factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate  + Altitude_Home
              + PM2.5_Home + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodalldx <- coeftest(xmodalld, vcov = vcovCL, cluster = ~RACE_Distance)

xmodallt <- lm(log(Distance+.001) ~ PM2.5*factor(Gender) + PM10*factor(Gender) + CO*factor(Gender) + NO2*factor(Gender) + O3*factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM2.5_Home + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodalltx <- coeftest(xmodallt, vcov = vcovCL, cluster = ~RACE_Distance)

xmod41d <- lm(log(Seconds+.001) ~ PM2.5*factor(Gender) + CO*factor(Gender) + NO2*factor(Gender) + O3*factor(Gender)
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home
             + PM2.5_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmod41dx <- coeftest(xmod41d, vcov = vcovCL, cluster = ~RACE_Distance)

xmod41t <- lm(log(Distance+.001) ~ PM2.5*factor(Gender) + CO*factor(Gender) + NO2*factor(Gender) + O3*factor(Gender)
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home
             + PM2.5_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmod41tx <- coeftest(xmod41t, vcov = vcovCL, cluster = ~RACE_Distance)

xmod42d <- lm(log(Seconds+.001) ~ PM10*factor(Gender) + CO*factor(Gender) + NO2*factor(Gender) + O3*factor(Gender)
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home
             + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmod42dx <- coeftest(xmod42d, vcov = vcovCL, cluster = ~RACE_Distance)

xmod42t <- lm(log(Distance+.001) ~ PM10*factor(Gender) + CO*factor(Gender) + NO2*factor(Gender) + O3*factor(Gender)
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home
             + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmod42tx <- coeftest(xmod42t, vcov = vcovCL, cluster = ~RACE_Distance)

# Results

write.csv(stargazer(xmodpmdx, xmodpm10dx, xmodcodx, xmodno2dx, xmodo3dx, xmodalldx, xmod41dx, xmod42dx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_db_X.txt', sep = ''))

write.csv(stargazer(xmodpmtx, xmodpm10tx, xmodcotx, xmodno2tx, xmodo3tx, xmodalltx, xmod41tx, xmod42tx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_tb_X.txt', sep = ''))

stargazer(xmodpmdx, xmodpm10dx, xmodcodx, xmodno2dx, xmodo3dx, xmodalldx, xmod41dx, xmod42dx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

stargazer(xmodpmtx, xmodpm10tx, xmodcotx, xmodno2tx, xmodo3tx, xmodalltx, xmod41tx, xmod42tx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

# Log-log models

xmodpmd2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodpmdxx <- coeftest(xmodpmd2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodpmt2 <- lm(log(Distance+.001) ~ log(PM2.5+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodpmtxx <- coeftest(xmodpmt2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodpm10d2 <- lm(log(Seconds+.001) ~ log(PM10+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodpm10dxx <- coeftest(xmodpm10d2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodpm10t2 <- lm(log(Distance+.001) ~ log(PM10+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodpm10txx <- coeftest(xmodpm10t2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodcod2 <- lm(log(Seconds+.001) ~ log(CO+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodcodxx <- coeftest(xmodcod2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodcot2 <- lm(log(Distance+.001) ~ log(CO+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodcotxx <- coeftest(xmodcot2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodno2d2 <- lm(log(Seconds+.001) ~ log(NO2+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodno2dxx <- coeftest(xmodno2d2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodno2t2 <- lm(log(Distance+.001) ~ log(NO2+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodno2txx <- coeftest(xmodno2t2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodo3d2 <- lm(log(Seconds+.001) ~ log(O3+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xmodo3dxx <- coeftest(xmodo3d2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodo3t2 <- lm(log(Distance+.001) ~ log(O3+.001)*factor(Gender) + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodo3txx <- coeftest(xmodo3t2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodalld2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001)*factor(Gender) + log(PM10+.001)*factor(Gender)
               + log(CO+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(O3+.001)*factor(Gender)
               + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home
               + PM2.5_Home + PM10_Home + CO_Home + NO2_Home + O3_Home,
               data = data[which(data$RACE_Distance %in% event_types_db),])

xmodalldxx <- coeftest(xmodalld2, vcov = vcovCL, cluster = ~RACE_Distance)

xmodallt2 <- lm(log(Distance+.001) ~ log(PM2.5+.001)*factor(Gender) + log(PM10+.001)*factor(Gender)
               + log(CO+.001)*factor(Gender) + log(NO2+.001)*factor(Gender) + log(O3+.001)*factor(Gender)
               + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home
               + PM2.5_Home + PM10_Home + CO_Home + NO2_Home + O3_Home,
               data = data[which(data$RACE_Distance %in% event_types_tb),])

xmodalltxx <- coeftest(xmodallt2, vcov = vcovCL, cluster = ~RACE_Distance)

xmod41d2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001)*factor(Gender) + log(CO+.001)*factor(Gender)
              + log(NO2+.001)*factor(Gender) + log(O3+.001)*factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM2.5_Home +CO_Home + NO2_Home + O3_Home,
              data = data[which(data$RACE_Distance %in% event_types_db),])

xmod41dxx <- coeftest(xmod41d2, vcov = vcovCL, cluster = ~RACE_Distance)

xmod41t2 <- lm(log(Distance+.001) ~ log(PM2.5+.001)*factor(Gender) + log(CO+.001)*factor(Gender)
              + log(NO2+.001)*factor(Gender) + log(O3+.001)*factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM2.5_Home + CO_Home + NO2_Home + O3_Home,
              data = data[which(data$RACE_Distance %in% event_types_tb),])

xmod41txx <- coeftest(xmod41t2, vcov = vcovCL, cluster = ~RACE_Distance)

xmod42d2 <- lm(log(Seconds+.001) ~ log(PM10+.001)*factor(Gender) + log(CO+.001)*factor(Gender)
              + log(NO2+.001)*factor(Gender) + log(O3+.001)*factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM10_Home + CO_Home + NO2_Home + O3_Home,
              data = data[which(data$RACE_Distance %in% event_types_db),])

xmod42dxx <- coeftest(xmod42d2, vcov = vcovCL, cluster = ~RACE_Distance)

xmod42t2 <- lm(log(Distance+.001) ~ log(PM10+.001)*factor(Gender) + log(CO+.001)*factor(Gender)
              + log(NO2+.001)*factor(Gender) + log(O3+.001)*factor(Gender)
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home
              + PM10_Home + CO_Home + NO2_Home + O3_Home,
              data = data[which(data$RACE_Distance %in% event_types_tb),])

xmod42txx <- coeftest(xmod42t2, vcov = vcovCL, cluster = ~RACE_Distance)

# Results

write.csv(stargazer(xmodpmdxx, xmodpm10dxx, xmodcodxx, xmodno2dxx, xmodo3dxx, xmodalldxx, xmod41dxx, xmod42dxx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_db_2_X.txt', sep = ''))

write.csv(stargazer(xmodpmtxx, xmodpm10txx, xmodcotxx, xmodno2txx, xmodo3txx, xmodalltxx, xmod41txx, xmod42txx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_tb_2_X.txt', sep = ''))

stargazer(xmodpmdxx, xmodpm10dxx, xmodcodxx, xmodno2dxx, xmodo3dxx, xmodalldxx, xmod41dxx, xmod42dxx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

stargazer(xmodpmtxx, xmodpm10txx, xmodcotxx, xmodno2txx, xmodo3txx, xmodalltxx, xmod41txx, xmod42txx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

# Repeating with a triple interaction between pollution and gender and ability

# Log-level models

xxxmodpmd <- lm(log(Seconds+.001) ~ PM2.5*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodpmdx <- coeftest(xxxmodpmd, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodpmt <- lm(log(Distance+.001) ~ PM2.5*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodpmtx <- coeftest(xxxmodpmt, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodpm10d <- lm(log(Seconds+.001) ~ PM10*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                  + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                  + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                  + Education_High_School + Education_Some_College + Education_Associates
                  + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodpm10dx <- coeftest(xxxmodpm10d, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodpm10t <- lm(log(Distance+.001) ~ PM10*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                  + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                  + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                  + Education_High_School + Education_Some_College + Education_Associates
                  + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodpm10tx <- coeftest(xxxmodpm10t, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodcod <- lm(log(Seconds+.001) ~ CO*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodcodx <- coeftest(xxxmodcod, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodcot <- lm(log(Distance+.001) ~ CO*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodcotx <- coeftest(xxxmodcot, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodno2d <- lm(log(Seconds+.001) ~ NO2*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodno2dx <- coeftest(xxxmodno2d, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodno2t <- lm(log(Distance+.001) ~ NO2*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodno2tx <- coeftest(xxxmodno2t, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodo3d <- lm(log(Seconds+.001) ~ O3*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodo3dx <- coeftest(xxxmodo3d, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodo3t <- lm(log(Distance+.001) ~ O3*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodo3tx <- coeftest(xxxmodo3t, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodalld <- lm(log(Seconds+.001) ~ PM2.5*factor(Gender)*Ability + PM10*factor(Gender)*Ability + CO*factor(Gender)*Ability + NO2*factor(Gender)*Ability + O3*factor(Gender)*Ability
                 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate  + Altitude_Home
                 + PM2.5_Home + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodalldx <- coeftest(xxxmodalld, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodallt <- lm(log(Distance+.001) ~ PM2.5*factor(Gender)*Ability + PM10*factor(Gender)*Ability + CO*factor(Gender)*Ability + NO2*factor(Gender)*Ability + O3*factor(Gender)*Ability
                 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home
                 + PM2.5_Home + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodalltx <- coeftest(xxxmodallt, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmod41d <- lm(log(Seconds+.001) ~ PM2.5*factor(Gender)*Ability + CO*factor(Gender)*Ability + NO2*factor(Gender)*Ability + O3*factor(Gender)*Ability
                + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home
                + PM2.5_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmod41dx <- coeftest(xxxmod41d, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmod41t <- lm(log(Distance+.001) ~ PM2.5*factor(Gender)*Ability + CO*factor(Gender)*Ability + NO2*factor(Gender)*Ability + O3*factor(Gender)*Ability
                + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home
                + PM2.5_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmod41tx <- coeftest(xxxmod41t, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmod42d <- lm(log(Seconds+.001) ~ PM10*factor(Gender)*Ability + CO*factor(Gender)*Ability + NO2*factor(Gender)*Ability + O3*factor(Gender)*Ability
                + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home
                + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmod42dx <- coeftest(xxxmod42d, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmod42t <- lm(log(Distance+.001) ~ PM10*factor(Gender)*Ability + CO*factor(Gender)*Ability + NO2*factor(Gender)*Ability + O3*factor(Gender)*Ability
                + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                + Education_High_School + Education_Some_College + Education_Associates
                + Education_Bachelors + Education_Graduate + Altitude_Home
                + PM10_Home + O3_Home + CO_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmod42tx <- coeftest(xxxmod42t, vcov = vcovCL, cluster = ~RACE_Distance)

# Results

write.csv(stargazer(xxxmodpmdx, xxxmodpm10dx, xxxmodcodx, xxxmodno2dx, xxxmodo3dx, xxxmodalldx, xxxmod41dx, xxxmod42dx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_db_XX.txt', sep = ''))

write.csv(stargazer(xxxmodpmtx, xxxmodpm10tx, xxxmodcotx, xxxmodno2tx, xxxmodo3tx, xxxmodalltx, xxxmod41tx, xxxmod42tx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_tb_XX.txt', sep = ''))

stargazer(xxxmodpmdx, xxxmodpm10dx, xxxmodcodx, xxxmodno2dx, xxxmodo3dx, xxxmodalldx, xxxmod41dx, xxxmod42dx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

stargazer(xxxmodpmtx, xxxmodpm10tx, xxxmodcotx, xxxmodno2tx, xxxmodo3tx, xxxmodalltx, xxxmod41tx, xxxmod42tx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

# Log-log models

xxxmodpmd2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodpmdxx <- coeftest(xxxmodpmd2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodpmt2 <- lm(log(Distance+.001) ~ log(PM2.5+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home + PM2.5_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodpmtxx <- coeftest(xxxmodpmt2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodpm10d2 <- lm(log(Seconds+.001) ~ log(PM10+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                   + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                   + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                   + Education_High_School + Education_Some_College + Education_Associates
                   + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodpm10dxx <- coeftest(xxxmodpm10d2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodpm10t2 <- lm(log(Distance+.001) ~ log(PM10+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                   + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                   + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                   + Education_High_School + Education_Some_College + Education_Associates
                   + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodpm10txx <- coeftest(xxxmodpm10t2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodcod2 <- lm(log(Seconds+.001) ~ log(CO+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodcodxx <- coeftest(xxxmodcod2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodcot2 <- lm(log(Distance+.001) ~ log(CO+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home + CO_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodcotxx <- coeftest(xxxmodcot2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodno2d2 <- lm(log(Seconds+.001) ~ log(NO2+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                  + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                  + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                  + Education_High_School + Education_Some_College + Education_Associates
                  + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodno2dxx <- coeftest(xxxmodno2d2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodno2t2 <- lm(log(Distance+.001) ~ log(NO2+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                  + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                  + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                  + Education_High_School + Education_Some_College + Education_Associates
                  + Education_Bachelors + Education_Graduate + Altitude_Home + NO2_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodno2txx <- coeftest(xxxmodno2t2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodo3d2 <- lm(log(Seconds+.001) ~ log(O3+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodo3dxx <- coeftest(xxxmodo3d2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodo3t2 <- lm(log(Distance+.001) ~ log(O3+.001)*factor(Gender)*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home + O3_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodo3txx <- coeftest(xxxmodo3t2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodalld2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001)*factor(Gender)*Ability + log(PM10+.001)*factor(Gender)*Ability
                  + log(CO+.001)*factor(Gender)*Ability + log(NO2+.001)*factor(Gender)*Ability + log(O3+.001)*factor(Gender)*Ability
                  + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                  + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                  + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                  + Education_High_School + Education_Some_College + Education_Associates
                  + Education_Bachelors + Education_Graduate + Altitude_Home
                  + PM2.5_Home + PM10_Home + CO_Home + NO2_Home + O3_Home,
                  data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmodalldxx <- coeftest(xxxmodalld2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmodallt2 <- lm(log(Distance+.001) ~ log(PM2.5+.001)*factor(Gender)*Ability + log(PM10+.001)*factor(Gender)*Ability
                  + log(CO+.001)*factor(Gender)*Ability + log(NO2+.001)*factor(Gender)*Ability + log(O3+.001)*factor(Gender)*Ability
                  + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                  + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                  + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                  + Education_High_School + Education_Some_College + Education_Associates
                  + Education_Bachelors + Education_Graduate + Altitude_Home
                  + PM2.5_Home + PM10_Home + CO_Home + NO2_Home + O3_Home,
                  data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmodalltxx <- coeftest(xxxmodallt2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmod41d2 <- lm(log(Seconds+.001) ~ log(PM2.5+.001)*factor(Gender)*Ability + log(CO+.001)*factor(Gender)*Ability
                 + log(NO2+.001)*factor(Gender)*Ability + log(O3+.001)*factor(Gender)*Ability
                 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home
                 + PM2.5_Home +CO_Home + NO2_Home + O3_Home,
                 data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmod41dxx <- coeftest(xxxmod41d2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmod41t2 <- lm(log(Distance+.001) ~ log(PM2.5+.001)*factor(Gender)*Ability + log(CO+.001)*factor(Gender)*Ability
                 + log(NO2+.001)*factor(Gender)*Ability + log(O3+.001)*factor(Gender)*Ability
                 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home
                 + PM2.5_Home + CO_Home + NO2_Home + O3_Home,
                 data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmod41txx <- coeftest(xxxmod41t2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmod42d2 <- lm(log(Seconds+.001) ~ log(PM10+.001)*factor(Gender)*Ability + log(CO+.001)*factor(Gender)*Ability
                 + log(NO2+.001)*factor(Gender)*Ability + log(O3+.001)*factor(Gender)*Ability
                 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home
                 + PM10_Home + CO_Home + NO2_Home + O3_Home,
                 data = data[which(data$RACE_Distance %in% event_types_db),])

xxxmod42dxx <- coeftest(xxxmod42d2, vcov = vcovCL, cluster = ~RACE_Distance)

xxxmod42t2 <- lm(log(Distance+.001) ~ log(PM10+.001)*factor(Gender)*Ability + log(CO+.001)*factor(Gender)*Ability
                 + log(NO2+.001)*factor(Gender)*Ability + log(O3+.001)*factor(Gender)*Ability
                 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                 + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                 + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                 + Education_High_School + Education_Some_College + Education_Associates
                 + Education_Bachelors + Education_Graduate + Altitude_Home
                 + PM10_Home + CO_Home + NO2_Home + O3_Home,
                 data = data[which(data$RACE_Distance %in% event_types_tb),])

xxxmod42txx <- coeftest(xxxmod42t2, vcov = vcovCL, cluster = ~RACE_Distance)

# Results

write.csv(stargazer(xxxmodpmdxx, xxxmodpm10dxx, xxxmodcodxx, xxxmodno2dxx, xxxmodo3dxx, xxxmodalldxx, xxxmod41dxx, xxxmod42dxx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_db_2_XX.txt', sep = ''))

write.csv(stargazer(xxxmodpmtxx, xxxmodpm10txx, xxxmodcotxx, xxxmodno2txx, xxxmodo3txx, xxxmodalltxx, xxxmod41txx, xxxmod42txx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_tb_2_XX.txt', sep = ''))

stargazer(xxxmodpmdxx, xxxmodpm10dxx, xxxmodcodxx, xxxmodno2dxx, xxxmodo3dxx, xxxmodalldxx, xxxmod41dxx, xxxmod42dxx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

stargazer(xxxmodpmtxx, xxxmodpm10txx, xxxmodcotxx, xxxmodno2txx, xxxmodo3txx, xxxmodalltxx, xxxmod41txx, xxxmod42txx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

# Sweet ass plots-in-table summary statistics

data$Gender_Female <- as.integer(data$Gender == 'F')
tmp <- data[,which(!names(data) %in% c('Altitude'))]
names(tmp)[names(tmp) == 'Altitude_Home'] <- 'Altitude'

tmp  <- subset(tmp, select = c('Distance', 'Seconds', 'PM2.5', 'PM10', 'CO', 'NO2', 'O3',
                               'Gender_Female', 'Age', 'Travel_Distance', 'In_State',
                               'Altitude', 'Total_Races', 'Overall', 'Age_Place',
                               'Gender_Place', 'RACE_Finisher_Count', 'RACE_Distance'))

names(tmp)[names(tmp) == 'Gender_Female'] <- 'Female'
names(tmp)[names(tmp) == 'Travel_Distance'] <- 'Travel Distance'
names(tmp)[names(tmp) == 'In_State'] <- 'In State'
names(tmp)[names(tmp) == 'Total_Races'] <- 'Total Races'
names(tmp)[names(tmp) == 'RACE_Finisher_Count'] <- 'Race Finisher Count'

tmpd <- tmp[,which(!names(tmp) %in% c('Distance'))]
tmpt <- tmp[,which(!names(tmp) %in% c('Seconds'))]

tmpd <- tmpd[which(tmpd$RACE_Distance %in% event_types_db),]
tmpt <- tmpt[which(tmpt$RACE_Distance %in% event_types_tb),]

tmpd <- tmpd[,which(!names(tmpd) %in% c('RACE_Distance', 'Gender_Place', 'Age', 'Overall', 'Age_Place'))]
tmpt <- tmpt[,which(!names(tmpt) %in% c('RACE_Distance', 'Gender_Place', 'Age', 'Overall', 'Age_Place'))]

datasummary_skim(tmpd)
datasummary_skim(tmpt)

# Testing to ensure that the pollution variables are indeed exogenous

# Reading in the matched data set

matches <- read.csv(paste(direc, 'exo_match.csv', sep = ''))
matches$Year <- floor(matches$Date / 10000)
matches$Month <- floor((matches$Date - 10000*matches$Year) / 100)

# Running regressions

exo.pm <- lm(PM2.5 ~ Count + factor(Year) + factor(Month) + factor(State), data = matches)
exo.pm10 <- lm(PM10 ~ Count + factor(Year) + factor(Month) + factor(State), data = matches)
exo.co <- lm(CO ~ Count + factor(Year) + factor(Month) + factor(State), data = matches)
exo.no2 <- lm(NO2 ~ Count + factor(Year) + factor(Month) + factor(State), data = matches)
exo.o3 <- lm(O3 ~ Count + factor(Year) + factor(Month) + factor(State), data = matches)

write.csv(stargazer(exo.pm, exo.pm10, exo.co, exo.no2, exo.o3, omit = c('Constant', 'Year', 'Month', 'State')), paste(direc2, 'endogeneity_test_results.txt', sep = ''))

stargazer(exo.pm, exo.pm10, exo.co, exo.no2, exo.o3, type = 'text', omit = c('Constant','Year', 'Month', 'State'))

# Map making

# Create a figure for runners per county in study window

plotdata <- data %>%
  group_by(FIPS_Runner) %>%
  count()

plotdata <- as.data.frame(plotdata)
names(plotdata)[1] <- 'id'

county_map
county_map$id <- as.numeric(county_map$id)

plotdata <- left_join(county_map, plotdata, by = 'id')

p1 <- ggplot(data = plotdata, mapping = aes(x = long, y = lat, fill = n, group = group))

p2 <- p1 + geom_polygon(color = NA)

p3 <- p2 + ggtitle('Number of Runners per County')

p4 <- p3 + theme(plot.title = element_text(hjust = 0.5))

p5 <- p4 + theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank())

p6 <- p5 + labs(fill = 'Runners')

p7 <- p6 + scale_fill_distiller(palette = 'RdYlGn')

# Create a figure for events per county in study window

plotdata2 <- data %>%
  group_by(FIPS_Race) %>%
  count(RACE_ID)

plotdata2 <- plotdata2 %>%
  group_by(FIPS_Race) %>%
  count()

plotdata2 <- as.data.frame(plotdata2)
names(plotdata2)[1] <- 'id'

county_map
county_map$id <- as.numeric(county_map$id)

plotdata2 <- left_join(county_map, plotdata2, by = 'id')

pp1 <- ggplot(data = plotdata2, mapping = aes(x = long, y = lat, fill = n, group = group))

pp2 <- pp1 + geom_polygon(color = NA)

pp3 <- pp2 + ggtitle('Number of Races per County')

pp4 <- pp3 + theme(plot.title = element_text(hjust = 0.5))

pp5 <- pp4 + theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())

pp6 <- pp5 + labs(fill = 'Races')

pp7 <- pp6 + scale_fill_distiller(palette = 'YlOrRd')

