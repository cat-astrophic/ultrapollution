# This script performs the singel gender econometric analyses for the ultrapollution project

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

# Subset for gender

dataf <- data[which(data$Gender == 'F'),]
datam <- data[which(data$Gender == 'M'),]

# Run the main models for women

modpmd <- lm(log(Seconds+.001) ~ PM2.5 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modpmdx <- coeftest(modpmd, vcov = vcovCL, cluster = ~RACE_Distance)

modpmt <- lm(log(Distance+.001) ~ PM2.5 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modpmtx <- coeftest(modpmt, vcov = vcovCL, cluster = ~RACE_Distance)

modpm10d <- lm(log(Seconds+.001) ~ PM10 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modpm10dx <- coeftest(modpm10d, vcov = vcovCL, cluster = ~RACE_Distance)

modpm10t <- lm(log(Distance+.001) ~ PM10 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modpm10tx <- coeftest(modpm10t, vcov = vcovCL, cluster = ~RACE_Distance)

modcod <- lm(log(Seconds+.001) ~ CO + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modcodx <- coeftest(modcod, vcov = vcovCL, cluster = ~RACE_Distance)

modcot <- lm(log(Distance+.001) ~ CO + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modcotx <- coeftest(modcot, vcov = vcovCL, cluster = ~RACE_Distance)

modno2d <- lm(log(Seconds+.001) ~ NO2 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modno2dx <- coeftest(modno2d, vcov = vcovCL, cluster = ~RACE_Distance)

modno2t <- lm(log(Distance+.001) ~ NO2 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modno2tx <- coeftest(modno2t, vcov = vcovCL, cluster = ~RACE_Distance)

modo3d <- lm(log(Seconds+.001) ~ O3 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modo3dx <- coeftest(modo3d, vcov = vcovCL, cluster = ~RACE_Distance)

modo3t <- lm(log(Distance+.001) ~ O3 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modo3tx <- coeftest(modo3t, vcov = vcovCL, cluster = ~RACE_Distance)

modalld <- lm(log(Seconds+.001) ~ PM2.5 + PM10 + CO + NO2 + O3
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate  + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modalldx <- coeftest(modalld, vcov = vcovCL, cluster = ~RACE_Distance)

modallt <- lm(log(Distance+.001) ~ PM2.5 + PM10 + CO + NO2 + O3
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modalltx <- coeftest(modallt, vcov = vcovCL, cluster = ~RACE_Distance)

mod41d <- lm(log(Seconds+.001) ~ PM2.5 + CO + NO2 + O3
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

mod41dx <- coeftest(mod41d, vcov = vcovCL, cluster = ~RACE_Distance)

mod41t <- lm(log(Distance+.001) ~ PM2.5 + CO + NO2 + O3
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

mod41tx <- coeftest(mod41t, vcov = vcovCL, cluster = ~RACE_Distance)

mod42d <- lm(log(Seconds+.001) ~ PM10 + CO + NO2 + O3
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

mod42dx <- coeftest(mod42d, vcov = vcovCL, cluster = ~RACE_Distance)

mod42t <- lm(log(Distance+.001) ~ PM10 + CO + NO2 + O3
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

mod42tx <- coeftest(mod42t, vcov = vcovCL, cluster = ~RACE_Distance)

# Run the main models for men

mmodpmd <- lm(log(Seconds+.001) ~ PM2.5 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodpmdx <- coeftest(mmodpmd, vcov = vcovCL, cluster = ~RACE_Distance)

mmodpmt <- lm(log(Distance+.001) ~ PM2.5 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodpmtx <- coeftest(mmodpmt, vcov = vcovCL, cluster = ~RACE_Distance)

mmodpm10d <- lm(log(Seconds+.001) ~ PM10 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodpm10dx <- coeftest(mmodpm10d, vcov = vcovCL, cluster = ~RACE_Distance)

mmodpm10t <- lm(log(Distance+.001) ~ PM10 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodpm10tx <- coeftest(mmodpm10t, vcov = vcovCL, cluster = ~RACE_Distance)

mmodcod <- lm(log(Seconds+.001) ~ CO + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodcodx <- coeftest(mmodcod, vcov = vcovCL, cluster = ~RACE_Distance)

mmodcot <- lm(log(Distance+.001) ~ CO + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodcotx <- coeftest(mmodcot, vcov = vcovCL, cluster = ~RACE_Distance)

mmodno2d <- lm(log(Seconds+.001) ~ NO2 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodno2dx <- coeftest(mmodno2d, vcov = vcovCL, cluster = ~RACE_Distance)

mmodno2t <- lm(log(Distance+.001) ~ NO2 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodno2tx <- coeftest(mmodno2t, vcov = vcovCL, cluster = ~RACE_Distance)

mmodo3d <- lm(log(Seconds+.001) ~ O3 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodo3dx <- coeftest(mmodo3d, vcov = vcovCL, cluster = ~RACE_Distance)

mmodo3t <- lm(log(Distance+.001) ~ O3 + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodo3tx <- coeftest(mmodo3t, vcov = vcovCL, cluster = ~RACE_Distance)

mmodalld <- lm(log(Seconds+.001) ~ PM2.5 + PM10 + CO + NO2 + O3
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate  + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodalldx <- coeftest(mmodalld, vcov = vcovCL, cluster = ~RACE_Distance)

mmodallt <- lm(log(Distance+.001) ~ PM2.5 + PM10 + CO + NO2 + O3
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodalltx <- coeftest(mmodallt, vcov = vcovCL, cluster = ~RACE_Distance)

mmod41d <- lm(log(Seconds+.001) ~ PM2.5 + CO + NO2 + O3
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmod41dx <- coeftest(mmod41d, vcov = vcovCL, cluster = ~RACE_Distance)

mmod41t <- lm(log(Distance+.001) ~ PM2.5 + CO + NO2 + O3
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmod41tx <- coeftest(mmod41t, vcov = vcovCL, cluster = ~RACE_Distance)

mmod42d <- lm(log(Seconds+.001) ~ PM10 + CO + NO2 + O3
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmod42dx <- coeftest(mmod42d, vcov = vcovCL, cluster = ~RACE_Distance)

mmod42t <- lm(log(Distance+.001) ~ PM10 + CO + NO2 + O3
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmod42tx <- coeftest(mmod42t, vcov = vcovCL, cluster = ~RACE_Distance)

# Results

write.csv(stargazer(modpmdx, mmodpmdx, modpm10dx, mmodpm10dx, modcodx, mmodcodx, modno2dx, mmodno2dx, modo3dx, mmodo3dx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'gender_results_db.txt', sep = ''))

write.csv(stargazer(modpmtx, mmodpmtx, modpm10tx, mmodpm10tx, modcotx, mmodcotx, modno2tx, mmodno2tx, modo3tx, mmodo3tx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'gender_results_tb.txt', sep = ''))

stargazer(modpmdx, mmodpmdx, modpm10dx, mmodpm10dx, modcodx, mmodcodx, modno2dx, mmodno2dx, modo3dx, mmodo3dx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

stargazer(modpmtx, mmodpmtx, modpm10tx, mmodpm10tx, modcotx, mmodcotx, modno2tx, mmodno2tx, modo3tx, mmodo3tx,
          omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

# Repeating with pollution x ability interaction

# Run the models for women

modpmd <- lm(log(Seconds+.001) ~ PM2.5*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modpmdx <- coeftest(modpmd, vcov = vcovCL, cluster = ~RACE_Distance)

modpmt <- lm(log(Distance+.001) ~ PM2.5*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modpmtx <- coeftest(modpmt, vcov = vcovCL, cluster = ~RACE_Distance)

modPM10d <- lm(log(Seconds+.001) ~ PM10*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                       + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                       + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                       + Education_High_School + Education_Some_College + Education_Associates
                       + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modPM10dx <- coeftest(modPM10d, vcov = vcovCL, cluster = ~RACE_Distance)

modPM10t <- lm(log(Distance+.001) ~ PM10*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                       + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                       + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                       + Education_High_School + Education_Some_College + Education_Associates
                       + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modPM10tx <- coeftest(modPM10t, vcov = vcovCL, cluster = ~RACE_Distance)

modCOd <- lm(log(Seconds+.001) ~ CO*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                     + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                     + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                     + Education_High_School + Education_Some_College + Education_Associates
                     + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modCOdx <- coeftest(modCOd, vcov = vcovCL, cluster = ~RACE_Distance)

modCOt <- lm(log(Distance+.001) ~ CO*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                     + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                     + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                     + Education_High_School + Education_Some_College + Education_Associates
                     + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modCOtx <- coeftest(modCOt, vcov = vcovCL, cluster = ~RACE_Distance)

modNO2d <- lm(log(Seconds+.001) ~ NO2*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                      + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                      + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                      + Education_High_School + Education_Some_College + Education_Associates
                      + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modNO2dx <- coeftest(modNO2d, vcov = vcovCL, cluster = ~RACE_Distance)

modNO2t <- lm(log(Distance+.001) ~ NO2*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                      + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                      + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                      + Education_High_School + Education_Some_College + Education_Associates
                      + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modNO2tx <- coeftest(modNO2t, vcov = vcovCL, cluster = ~RACE_Distance)

modO3d <- lm(log(Seconds+.001) ~ O3*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                     + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                     + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                     + Education_High_School + Education_Some_College + Education_Associates
                     + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modO3dx <- coeftest(modO3d, vcov = vcovCL, cluster = ~RACE_Distance)

modO3t <- lm(log(Distance+.001) ~ O3*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                     + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                     + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                     + Education_High_School + Education_Some_College + Education_Associates
                     + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modO3tx <- coeftest(modO3t, vcov = vcovCL, cluster = ~RACE_Distance)

modalld <- lm(log(Seconds+.001) ~ PM2.5*Ability + PM10*Ability + CO*Ability + NO2*Ability + O3*Ability
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate  + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

modalldx <- coeftest(modalld, vcov = vcovCL, cluster = ~RACE_Distance)

modallt <- lm(log(Distance+.001) ~ PM2.5*Ability + PM10*Ability + CO*Ability + NO2*Ability + O3*Ability
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

modalltx <- coeftest(modallt, vcov = vcovCL, cluster = ~RACE_Distance)

mod41d <- lm(log(Seconds+.001) ~ PM2.5*Ability + CO*Ability + NO2*Ability + O3*Ability
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

mod41dx <- coeftest(mod41d, vcov = vcovCL, cluster = ~RACE_Distance)

mod41t <- lm(log(Distance+.001) ~ PM2.5*Ability + CO*Ability + NO2*Ability + O3*Ability
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

mod41tx <- coeftest(mod41t, vcov = vcovCL, cluster = ~RACE_Distance)

mod42d <- lm(log(Seconds+.001) ~ PM10*Ability + CO*Ability + NO2*Ability + O3*Ability
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_db),])

mod42dx <- coeftest(mod42d, vcov = vcovCL, cluster = ~RACE_Distance)

mod42t <- lm(log(Distance+.001) ~ PM10*Ability + CO*Ability + NO2*Ability + O3*Ability
             + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = dataf[which(data$RACE_Distance %in% event_types_tb),])

mod42tx <- coeftest(mod42t, vcov = vcovCL, cluster = ~RACE_Distance)

# Run the main models for men

mmodpmd <- lm(log(Seconds+.001) ~ PM2.5*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodpmdx <- coeftest(mmodpmd, vcov = vcovCL, cluster = ~RACE_Distance)

mmodpmt <- lm(log(Distance+.001) ~ PM2.5*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodpmtx <- coeftest(mmodpmt, vcov = vcovCL, cluster = ~RACE_Distance)

mmodPM10d <- lm(log(Seconds+.001) ~ PM10*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                        + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                        + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                        + Education_High_School + Education_Some_College + Education_Associates
                        + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodPM10dx <- coeftest(mmodPM10d, vcov = vcovCL, cluster = ~RACE_Distance)

mmodPM10t <- lm(log(Distance+.001) ~ PM10*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                        + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                        + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                        + Education_High_School + Education_Some_College + Education_Associates
                        + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodPM10tx <- coeftest(mmodPM10t, vcov = vcovCL, cluster = ~RACE_Distance)

mmodCOd <- lm(log(Seconds+.001) ~ CO*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                      + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                      + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                      + Education_High_School + Education_Some_College + Education_Associates
                      + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodCOdx <- coeftest(mmodCOd, vcov = vcovCL, cluster = ~RACE_Distance)

mmodCOt <- lm(log(Distance+.001) ~ CO*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                      + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                      + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                      + Education_High_School + Education_Some_College + Education_Associates
                      + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodCOtx <- coeftest(mmodCOt, vcov = vcovCL, cluster = ~RACE_Distance)

mmodNO2d <- lm(log(Seconds+.001) ~ NO2*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                       + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                       + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                       + Education_High_School + Education_Some_College + Education_Associates
                       + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodNO2dx <- coeftest(mmodNO2d, vcov = vcovCL, cluster = ~RACE_Distance)

mmodNO2t <- lm(log(Distance+.001) ~ NO2*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                       + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                       + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                       + Education_High_School + Education_Some_College + Education_Associates
                       + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodNO2tx <- coeftest(mmodNO2t, vcov = vcovCL, cluster = ~RACE_Distance)

mmodO3d <- lm(log(Seconds+.001) ~ O3*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                      + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                      + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                      + Education_High_School + Education_Some_College + Education_Associates
                      + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodO3dx <- coeftest(mmodO3d, vcov = vcovCL, cluster = ~RACE_Distance)

mmodO3t <- lm(log(Distance+.001) ~ O3*Ability + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
                      + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
                      + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
                      + Education_High_School + Education_Some_College + Education_Associates
                      + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodO3tx <- coeftest(mmodO3t, vcov = vcovCL, cluster = ~RACE_Distance)

mmodalld <- lm(log(Seconds+.001) ~ PM2.5*Ability + PM10*Ability + CO*Ability + NO2*Ability + O3*Ability
               + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate  + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmodalldx <- coeftest(mmodalld, vcov = vcovCL, cluster = ~RACE_Distance)

mmodallt <- lm(log(Distance+.001) ~ PM2.5*Ability + PM10*Ability + CO*Ability + NO2*Ability + O3*Ability
               + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmodalltx <- coeftest(mmodallt, vcov = vcovCL, cluster = ~RACE_Distance)

mmod41d <- lm(log(Seconds+.001) ~ PM2.5*Ability + CO*Ability + NO2*Ability + O3*Ability
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmod41dx <- coeftest(mmod41d, vcov = vcovCL, cluster = ~RACE_Distance)

mmod41t <- lm(log(Distance+.001) ~ PM2.5*Ability + CO*Ability + NO2*Ability + O3*Ability
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmod41tx <- coeftest(mmod41t, vcov = vcovCL, cluster = ~RACE_Distance)

mmod42d <- lm(log(Seconds+.001) ~ PM10*Ability + CO*Ability + NO2*Ability + O3*Ability
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_db),])

mmod42dx <- coeftest(mmod42d, vcov = vcovCL, cluster = ~RACE_Distance)

mmod42t <- lm(log(Distance+.001) ~ PM10*Ability + CO*Ability + NO2*Ability + O3*Ability
              + Temperature*Humidity + Precipitation + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = datam[which(data$RACE_Distance %in% event_types_tb),])

mmod42tx <- coeftest(mmod42t, vcov = vcovCL, cluster = ~RACE_Distance)

# Results

write.csv(stargazer(modpmdx, mmodpmdx, modPM10dx, mmodPM10dx, modCOdx, mmodCOdx, modNO2dx, mmodNO2dx, modO3dx, mmodO3dx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'gender_results_db_XX.txt', sep = ''))

write.csv(stargazer(modpmtx, mmodpmtx, modPM10tx, mmodPM10tx, modCOtx, mmodCOtx, modNO2tx, mmodNO2tx, modO3tx, mmodO3tx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'gender_results_tb_XX.txt', sep = ''))

stargazer(modpmdx, mmodpmdx, modPM10dx, mmodPM10dx, modCOdx, mmodCOdx, modNO2dx, mmodNO2dx, modO3dx, mmodO3dx,
                    omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')
          
stargazer(modpmtx, mmodpmtx, modPM10tx, mmodPM10tx, modCOtx, mmodCOtx, modNO2tx, mmodNO2tx, modO3tx, mmodO3tx,
                              omit = c('FIPS_Race', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

