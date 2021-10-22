# This script performs the econometric analyses for the ultrapollution project

# Loading libraries

library(stargazer)
library(sandwich)
library(ggplot2)
library(jtools)
library(dplyr)
library(kableExtra)
library(modelsummary)

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

# Run the main models

modpmd <- lm(log(Seconds) ~ PM2.5 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

cov <- vcovHC(modpmd, type = 'HC0')
hrsepmd <- sqrt(abs(diag(cov)))

modpmt <- lm(log(Distance) ~ PM2.5 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

cov <- vcovHC(modpmt, type = 'HC0')
hrsepmt <- sqrt(abs(diag(cov)))

modpm10d <- lm(log(Seconds) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

cov <- vcovHC(modpm10d, type = 'HC0')
hrsepm10d <- sqrt(abs(diag(cov)))

modpm10t <- lm(log(Distance) ~ PM10 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
               + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
               + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
               + Education_High_School + Education_Some_College + Education_Associates
               + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

cov <- vcovHC(modpm10t, type = 'HC0')
hrsepm10t <- sqrt(abs(diag(cov)))

modcod <- lm(log(Seconds) ~ CO + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

cov <- vcovHC(modcod, type = 'HC0')
hrsecod <- sqrt(abs(diag(cov)))

modcot <- lm(log(Distance) ~ CO + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

cov <- vcovHC(modcot, type = 'HC0')
hrsecot <- sqrt(abs(diag(cov)))

modno2d <- lm(log(Seconds) ~ NO2 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

cov <- vcovHC(modno2d, type = 'HC0')
hrseno2d <- sqrt(abs(diag(cov)))

modno2t <- lm(log(Distance) ~ NO2 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

cov <- vcovHC(modno2t, type = 'HC0')
hrseno2t <- sqrt(abs(diag(cov)))

modo3d <- lm(log(Seconds) ~ O3 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

cov <- vcovHC(modo3d, type = 'HC0')
hrseo3d <- sqrt(abs(diag(cov)))

modo3t <- lm(log(Distance) ~ O3 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

cov <- vcovHC(modo3t, type = 'HC0')
hrseo3t <- sqrt(abs(diag(cov)))

modalld <- lm(log(Seconds) ~ PM2.5 + PM10 + CO + NO2 + O3 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

cov <- vcovHC(modalld, type = 'HC0')
hrsealld <- sqrt(abs(diag(cov)))

modallt <- lm(log(Distance) ~ PM2.5 + PM10 + CO + NO2 + O3 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
              + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

cov <- vcovHC(modallt, type = 'HC0')
hrseallt <- sqrt(abs(diag(cov)))

mod41d <- lm(log(Seconds) ~ PM2.5 + CO + NO2 + O3 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

cov <- vcovHC(mod41d, type = 'HC0')
hrse41d <- sqrt(abs(diag(cov)))

mod41t <- lm(log(Distance) ~ PM2.5 + CO + NO2 + O3 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

cov <- vcovHC(mod41t, type = 'HC0')
hrse41t <- sqrt(abs(diag(cov)))

mod42d <- lm(log(Seconds) ~ PM10 + CO + NO2 + O3 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_db),])

cov <- vcovHC(mod42d, type = 'HC0')
hrse42d <- sqrt(abs(diag(cov)))

mod42t <- lm(log(Distance) ~ PM10 + CO + NO2 + O3 + factor(FIPS_Race) + factor(RACE_Name) + factor(RACE_Month)*factor(RACE_Year)
             + factor(RACE_Distance) + factor(Gender) + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home, data = data[which(data$RACE_Distance %in% event_types_tb),])

cov <- vcovHC(mod42t, type = 'HC0')
hrse42t <- sqrt(abs(diag(cov)))

# Results

write.csv(stargazer(modpmd, modpm10d, modcod, modno2d, modo3d, modalld, mod41d, mod42d,
                    se = list(hrsepmd, hrsepm10d, hrsecod, hrseno2d, hrseo3d, hrsealld, hrse41d, hrse42d),
                    omit = c('FIPS_Race', 'RACE_Name', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_db.txt', sep = ''))

write.csv(stargazer(modpmt, modpm10t, modcot, modno2t, modo3t, modallt, mod41t, mod42t,
                    se = list(hrsepmt, hrsepm10t, hrsecot, hrseno2t, hrseo3t, hrseallt, hrse41t, hrse42t),
                    omit = c('FIPS_Race', 'RACE_Name', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser')),
          paste(direc2, 'results_tb.txt', sep = ''))

stargazer(modpmd, modpm10d, modcod, modno2d, modo3d, modalld, mod41d, mod42d,
          se = list(hrsepmd, hrsepm10d, hrsecod, hrseno2d, hrseo3d, hrsealld, hrse41d, hrse42d),
          omit = c('FIPS_Race', 'RACE_Name', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

stargazer(modpmt, modpm10t, modcot, modno2t, modo3t, modallt, mod41t, mod42t,
          se = list(hrsepmt, hrsepm10t, hrsecot, hrseno2t, hrseo3t, hrseallt, hrse41t, hrse42t),
          omit = c('FIPS_Race', 'RACE_Name', 'RACE_Month', 'RACE_Year', 'RACE_Distance'), omit.stat = c('f', 'ser'), type = 'text')

# Writing data for computing t-statistics to file because stargazer did not allow for gazing at the stars in modpmd and modpmt...

write.csv(modpmd$coefficients, paste(direc2, 'modpmd$coefficients.txt', sep = ''), row.names = TRUE)
write.csv(modpmt$coefficients, paste(direc2, 'modpmt$coefficients.txt', sep = ''), row.names = TRUE)
write.csv(hrsepmt, paste(direc2, 'hrsepmd.txt', sep = ''), row.names = TRUE)
write.csv(hrsepmd, paste(direc2, 'hrsepmt.txt', sep = ''), row.names = TRUE)

# Sweet ass plots-in-table summary statistics

data$Gender_Female <- as.integer(data$Gender == 'F')
tmp <- data[,which(!names(data) %in% c('Altitude'))]
names(tmp)[names(tmp) == 'Altitude_Home'] <- 'Altitude'

tmp  <- subset(tmp, select = c('Distance', 'Seconds', 'PM2.5', 'PM10', 'CO', 'NO2', 'O3',
                               'Gender_Female', 'Age', 'Travel_Distance', 'In_State',
                               'Altitude', 'Total_Races', 'Overall', 'Age_Place',
                               'Gender_Place', 'RACE_Finisher_Count', 'RACE_Distance'))

tmpd <- tmp[,which(!names(tmp) %in% c('Distance'))]
tmpt <- tmp[,which(!names(tmp) %in% c('Seconds'))]

tmpd <- tmpd[which(tmpd$RACE_Distance %in% event_types_db),]
tmpt <- tmpt[which(tmpt$RACE_Distance %in% event_types_tb),]

tmpd <- tmpd[,which(!names(tmpd) %in% c('RACE_Distance'))]
tmpt <- tmpt[,which(!names(tmpt) %in% c('RACE_Distance'))]

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

