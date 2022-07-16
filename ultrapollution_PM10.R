# This script performs the econometric analyses for the ultrapollution project

# Loading libraries

library(stargazer)
library(sandwich)
library(lmtest)

# Specifying directories for data + results

direc <- paste('C:/Users/', username, '/Documents/Data/ultrapollution/ultra_data/', sep = '')
direc2 <- paste('C:/Users/', username, '/Documents/Data/ultrapollution/results/', sep = '')

# Reading in the data set

data <- read.csv(paste(direc, 'ultradata.csv', sep = ''))

# See which event types have at least 100 observations

event_types <- c()
for (d in unique(data$RACE_Distance)) {if (dim(data[which(data$RACE_Distance == d),][1])>100) {event_types <- c(event_types,d)}}

# Run OLS regressions

mod50k <- lm(log(Seconds+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation
             + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home,
             data = data[which(data$RACE_Distance == '50 KM'),])

mod50m <- lm(log(Seconds+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation
             + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home,
             data = data[which(data$RACE_Distance == '50 Miles'),])

mod100k <- lm(log(Seconds+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation
              + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home,
              data = data[which(data$RACE_Distance == '100 KM'),])

mod100m <- lm(log(Seconds+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation
              + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
              + Total_Races + RACE_Finisher_Count + In_State
              + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
              + Education_High_School + Education_Some_College + Education_Associates
              + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home,
              data = data[which(data$RACE_Distance == '100 Miles'),])

mod6h <- lm(log(Distance+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation
            + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
            + Total_Races + RACE_Finisher_Count + In_State
            + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
            + Education_High_School + Education_Some_College + Education_Associates
            + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home,
            data = data[which(data$RACE_Distance == '6 Hours'),])

mod12h <- lm(log(Distance+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation
             + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home,
             data = data[which(data$RACE_Distance == '12 Hours'),])

mod24h <- lm(log(Distance+.001) ~ PM10 + factor(Gender) + Temperature*Humidity + Precipitation
             + WindSpeed + factor(FIPS_Race) + factor(RACE_Month)*factor(RACE_Year)
             + Total_Races + RACE_Finisher_Count + In_State
             + Travel_Distance + Ability + Population + Poverty_Rate + Unemployment_Rate + Income
             + Education_High_School + Education_Some_College + Education_Associates
             + Education_Bachelors + Education_Graduate + Altitude_Home + PM10_Home,
             data = data[which(data$RACE_Distance == '24 Hours'),])

xmod50k <- coeftest(mod50k, vcov = vcovCL, cluster = ~RACE_Name)
xmod50m <- coeftest(mod50m, vcov = vcovCL, cluster = ~RACE_Name)
xmod100k <- coeftest(mod100k, vcov = vcovCL, cluster = ~RACE_Name)
xmod100m <- coeftest(mod100m, vcov = vcovCL, cluster = ~RACE_Name)
xmod6h <- coeftest(mod6h, vcov = vcovCL, cluster = ~RACE_Name)
xmod12h <- coeftest(mod12h, vcov = vcovCL, cluster = ~RACE_Name)
xmod24h <- coeftest(mod24h, vcov = vcovCL, cluster = ~RACE_Name)

# Viewing results

write.csv(stargazer(xmod50k, xmod50m, xmod100k, xmod100m, xmod6h, xmod12h, xmod24h,
                    omit = c('FIPS_Race', 'RACE_Name', 'RACE_Month', 'RACE_Year')), 
          paste(direc2, 'PM10_results_tex.txt', sep = ''), row.names = FALSE)

write.csv(stargazer(xmod50k, xmod50m, xmod100k, xmod100m, xmod6h, xmod12h, xmod24h, type = 'text',
                    omit = c('FIPS_Race', 'RACE_Name', 'RACE_Month', 'RACE_Year')), 
          paste(direc2, 'PM10_results.txt', sep = ''), row.names = FALSE)

