# this script assumes we've run something like xml-loading-parsing.R
# and built a set of tables, one of lap data and
# one of trackpoint data, for each rider out of their TCX files.
library(signal)
library(dplyr)
library(ggplot2)
library(tidyr)

library(lubridate)

# assumes we have data tables called dlap, klap, dtrackpoints, ktrackpoints
# this round brute-forces repeated activities over both files 
# next step is to put a more generic looping wrapper around it

ktrackpoints <- mutate(ktrackpoints, Rider = "K")
dtrackpoints <- mutate(dtrackpoints, Rider = "D")
#not sure this next bit is necessary but want to be sure cols are ordered same
ktrackpoints <- select(ktrackpoints, Time, DistanceMeters, HeartRateBpm,
                       Cadence, Watts, LatitudeDegrees, LongitudeDegrees, Rider)
dtrackpoints <- select(dtrackpoints, Time, DistanceMeters, HeartRateBpm,
                       Cadence, Watts, LatitudeDegrees, LongitudeDegrees, Rider)


data <- bind_rows(ktrackpoints, dtrackpoints)
cleandata <- data[complete.cases(data), ]

cleandata$Time <- ymd_hms(cleandata$Time, tz = "America/Detroit")
cleandata$DistanceMeters <- as.numeric(cleandata$DistanceMeters)
cleandata$HeartRateBpm <- as.numeric(cleandata$HeartRateBpm)
cleandata$Cadence <- as.numeric(cleandata$Cadence)
cleandata$LatitudeDegrees <- as.numeric(cleandata$LatitudeDegrees)
cleandata$LongitudeDegrees <- as.numeric(cleandata$LongitudeDegrees)

cleandata <- dplyr::mutate(cleandata, distdiff = DistanceMeters - lag(DistanceMeters))

# smooth the power

bf <- butter(2, 1/50, type = "low")
cleandata$filtered.watts[cleandata$Rider == "D"] <- signal::filter(bf, cleandata$Watts[cleandata$Rider == "D"])
cleandata$filtered.watts[cleandata$Rider == "K"] <- signal::filter(bf, cleandata$Watts[cleandata$Rider == "K"])

#this is a test plot to see what we have in the raw stuff
par(mfrow = c(1,1))
plot(dtrackpoints$Time, dtrackpoints$HeartRateBpm, type = "l", col = "red")
lines(cleandata$Time[cleandata$Rider == "D"], cleandata$filtered.watts[cleandata$Rider == "D"], col = "green")
lines(cleandata$Time[cleandata$Rider == "K"], cleandata$filtered.watts[cleandata$Rider == "K"])


# so now let's take this raw info and start making something out of it.
# build a table of minute data
minutebyminute <- cleandata %>% 
        dplyr::mutate(Time = floor_date(Time, "minute")) %>%
        dplyr::group_by(Rider, Time) %>% 
        dplyr::summarise(minute.watts = mean(filtered.watts))

# calculate the time difference from one line to the next to find pauses. 
# while we're at it, work out distance diff too and stuff them in cols that can be
# pasted onto the minutebyminute table

diffs <- cleandata %>% 
        dplyr::mutate(Time = floor_date(Time, "minute")) %>%
        dplyr::group_by(Rider, Time) %>% 
        dplyr::summarize(minute.dist = sum(distdiff)) %>%
        dplyr::mutate(timediff = as.numeric(seconds(Time - lag(Time))))

diffs$timediff[diffs$timediff < 0] = 0
diffs$timepause <- ifelse(diffs$timediff > 1,"Pause before", "Moving")
diffs$minute.dist[diffs$minute.dist < 0] <- 0
diffs$distpause <- ifelse(diffs$minute.dist > 100, "Moving", "Stopped")

#glue them together now

minutebyminute <- bind_cols(minutebyminute, select(diffs, minute.dist, timediff, timepause, distpause))

# strictly for the sake of tidyness in this experiment, lopping off post-Chelsea:

cropped <- minutebyminute[minutebyminute$Time < ymd_hms("2015-05-24 14:00:00", tz = "America/Detroit"), ]


names(cropped)[3] <- "Watts"
wide <- spread(cropped, Rider, Watts)
wide[is.na(wide)] <- 0
wide <-  dplyr::summarise(group_by(wide, Time), D = sum(D), K = sum(K))

wide <- mutate(wide, diff = D - K)
wide <- mutate(wide, kup = ifelse(diff < 0, abs(diff), 0))
wide <- mutate(wide, dup = ifelse(diff > 0, abs(diff), 0))
wide$dup <- -wide$dup

narrow <- wide %>% gather(riderup, value, kup:dup)

ggplot(narrow, aes(Time, value)) + geom_area(aes(fill = riderup))

#how many minutes was someone up?

timetable <- table(narrow$riderup[narrow$value != 0])
timetable
print(paste("Total Rolling Minutes:", sum(timetable)))
kup <- timetable[1]
dup<- timetable[2]
kpct <- kup/(sum(timetable))*100
dpct <- dup/sum(timetable)*100
print(paste("K up:", kpct, "%  /  D up:", dpct))

#what does this look like in base plotting?
plot(wide$Time, wide$kup, type = "l", ylim = c(-100, 50))
lines(wide$Time, wide$dup, col = "red")

