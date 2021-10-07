# Main script for a paper on athletes, gender, and risk

# Specifying the directory for the data and results -- ensure that the directory is correct

username <- ''
direc <- paste('C:/Users/', username, '/Documents/Data/ultrapollution/ultra_data/', sep = '')
direc2 <- paste('C:/Users/', username, '/Documents/Data/ultrapollution/results/', sep = '')

# Running the individual scripts for each set of analyses -- ensure that the directory is correct

diRectoRy <- paste('C:/Users/', username, '/Documents/', sep = '')

# Running the R scripts

source(paste(diRectoRy, 'ultrapollution_PM25.R', sep = ''))
source(paste(diRectoRy, 'ultrapollution_PM10.R', sep = ''))
source(paste(diRectoRy, 'ultrapollution_O3.R', sep = ''))
source(paste(diRectoRy, 'ultrapollution_CO.R', sep = ''))
source(paste(diRectoRy, 'ultrapollution_NO2.R', sep = ''))
source(paste(diRectoRy, 'ultrapollution_complete.R', sep = ''))

