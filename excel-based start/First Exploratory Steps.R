# rough list of approximately how I got to that first plot... 
# Do not expect this to run.

library(XML)
library(dplyr)
library(ggplot2)
library(signal)
library(lubridate)

library("signal", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
dwg <- read.csv(file = "dwg_ride.csv")
kgw <- read.csv(file = "kgw_ride.csv")

dwg <- mutate(dwg, parsed_time = ymd_hms(Time, tz = "America/Detroit"))
kgw <- mutate(kgw, parsed_time = ymd_hms(Time, tz = "America/Detroit"))
kgw <- mutate(kgw, rider = "K")
dwg <- mutate(dwg, rider = "D")
data <- bind_rows(kgw, dwg)
cleandata <- filter(data, !is.na(Watts))

#made a test data set to play with (details not here)

bf <- butter(2, 1/50, type = "low")
dwg <- dplyr::filter(dwg, !is.na(Watts))
dwg <- mutate(dwg, filteredWatts <- filter(bf, Watts))
df <- filter(bf, dwg$Watts)
kf <- filter(bf, kgw$Watts)
df <- data.frame(df)
bind_rows(dwg, df)
names(df) <- "filtered.watts"
kf <- data.frame(kf)
names(kf) <- "filtered.watts"
bind_rows(kgw, kf)
data <- bind_rows(kgw, dwg)
minutebyminute <- data %>% group_by(rider, hour(parsed_time), minute(parsed_time)) %>% summarise(minute.watts <- mean(filtered.watts))
minutebyminute <- mutate(minutebyminute, shorttime <- hm(paste(hour,minute)))
minutebyminute <- mutate(minutebyminute, summary_time = now())
day(minutebyminute$summary_time) <- 15
hour(minutebyminute$summary_time) <- minutebyminute$hour(parsed_time)
mutate(minutebyminute, hour(summary_time) = hour(parsed_time))
names(minutebyminute)[2:3] = c("hr","min")
mutate(minutebyminute, sumtime = paste("2015-05-15",as.character(hr), ":", as.character(min),":00", sep = ""))
sumtime <- paste("2015-05-15 ",as.character(minutebyminute$hr), ":", as.character(minutebyminute$min),":00", sep = "")
sumtimep <- ymd_hms(sumtime)
bind_cols(minutebyminute, sumtimep)
data.frame(sumtimep)
sumtime <- data.frame(sumtimep)
rm(sumtimep)
minutebyminute <- bind_cols(minutebyminute, sumtime)
names(minutebyminute)[4] <- "watts"
wide <- spread(minutebyminute, rider, watts)
wide[is.na(wide)] <- 0
summarise(wide, sumtimep, d = sum(D), k = sum(K))
wides <- wide %>% group_by(sumtimep) %>% summarise_each(funs(sum))
wide <- select(wides, sumtimep, D, K)
wide <- mutate(wide, diff = D - K)
wide <- mutate(wide, kup = ifelse(diff < 0, abs(diff), 0))
wide <- mutate(wide, dup = ifelse(diff > 0, abs(diff), 0))
narrow <- gather(wide, delta, kup:dup)
wide %>% gather(riderup, value, kup:dup)
narrow <- wide %>% gather(riderup, value, kup:dup)
narrow[662,6] <- 0
narrow[391,6] <- 0
narrow[392,6] <- 0
ggplot(narrow, aes(sumtimep, value)) + geom_area(aes(fill = riderup))

