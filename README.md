# ultrapollution

this repo is for a project on pollution and physical productivity and gender which is one of my dissertation chapters - it is currently under review at *JAERE*

the NOAA data comes from [my NOAA weather data repo](https://github.com/cat-astrophic/NOAA)

the pollution data script is currently set up to only collect PM10 data - the final data set is still ~1gb

these scripts can easily be adjusted for other pollutants (each will give a ~1gb output)

the reason for only running one pollutant at a time is that not every file came through the API and so I manually pulled the data from the EPA AQS and inserted them into the data parsing workflow for each pollutant


