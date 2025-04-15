library(nasapower)
library(tidyverse)
library(RAQSAPI)
# need to set aqs credentials

# Marathon results taken from here 
# https://www.kaggle.com/datasets/runningwithrock/2023-marathon-results?resource=download
df <- read.csv("Races.csv")
df$Date <- as.POSIXlt(df$Date, format = "%m/%d/%y")

# Weather function - pulls from NASA Power database
get_weather <- function(lat, lng, date){
  res <- get_power(lonlat = c(lng, lat), 
            pars = c("RH2M", 
                     "T2M",
                     "T2M_MIN",
                     "T2M_MAX",
                     "T2MDEW",
                     "T2MWET",
                     "WS2M",
                     "WS2M_MIN",
                     "WS2M_MAX",
                     "ALLSKY_SFC_UV_INDEX",
                     "CLOUD_AMT_DAY",
                     "PRECTOTCORR"), 
            temporal_api = "daily", 
            dates = date)
  return(res)
}

# Get all weather data for each race lat/lng
res <- get_weather(df$Latitude[1], df$Longitude[1], df$Date[1])
for (i in 2:nrow(df)){
  print(i)
  res <- rbind(res, get_weather(df$Latitude[i], 
                                df$Longitude[i], 
                                df$Date[i]))
}
write.csv(res, "weather.csv", row.names = FALSE)

# Use AQS database for PM2.5 data
param <- c("88101")

# Used to create bounding box around each rate point
distance <- 10*1609 # 10 miles*1609m
deltaLatitude = (180/pi)*(distance / 6371000)
deltaLongitude = (180/pi)*(distance /
                             (cos(df$Latitude[1]*pi/180) * 6371000))

# empty results data frame
df$mean_pm25 <- NA
df$median_pm25 <- NA
df$max_pm25 <- NA
df$min_pm25 <- NA

# loop through dates/marathons
for(i in 1:nrow(df)){
  print(i)
  deltaLatitude = (180/pi)*(distance / 6371000)
  deltaLongitude = (180/pi)*(distance / (cos(df$Latitude[i]*pi/180) * 6371000))
  
  aqi_res <- aqs_dailysummary_by_box(param,
                          bdate = df$Date[i],
                          edate = df$Date[i],
                          minlat = df$Latitude[i]-deltaLatitude, 
                          maxlat = df$Latitude[i]+deltaLatitude, 
                          minlon = df$Longitude[i]-deltaLongitude, 
                          maxlon = df$Latitude[i]+deltaLongitude) 
    
  # can return an empty data frame so need to check
  if (nrow(aqi_res) > 0){
    aqi_res <- aqi_res %>%
      filter(sample_duration == "24-HR BLK AVG")
    if (nrow(aqi_res) >0){
      df$mean_pm25[i] <- mean(aqi_res$arithmetic_mean, na.rm=TRUE)
      df$median_pm25[i] <- median(aqi_res$arithmetic_mean, na.rm=TRUE)
      df$max_pm25[i] <- max(aqi_res$arithmetic_mean, na.rm=TRUE)
      df$min_pm25[i] <- min(aqi_res$arithmetic_mean, na.rm=TRUE)
    }
  }
}
write.csv(df, "88101.csv", row.names = FALSE)


