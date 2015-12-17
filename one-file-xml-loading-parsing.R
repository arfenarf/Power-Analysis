# Second pass, now importing TCX files into tables directly
# No fancy file handling yet or anything

require(XML)
require(dplyr)
require(lubridate)
print(now())

# first:  a lap parsing FUN:

# another idea from http://stackoverflow.com/questions/16805050/extract-xml-node-and-attribute-in-r
# lightning fast, but does something really weird if run over Trackpoints.

Fun2 <-function(rootnode){
        dumFun <- function(x){
                xname <- xmlName(x)
                xattrs <- xmlAttrs(x)
                c(sapply(xmlChildren(x), xmlValue), name = xname, xattrs)
        }
        
        as.data.frame(t(xpathSApply(rootnode, "//*/ns:Lap", namespaces = ns,dumFun)), stringsAsFactors = FALSE)
}

#load the data
kgw_lr_root <- xmlRoot(xmlInternalTreeParse("tcxfiles//20150524-kgw-b.tcx"))
dwg_lr_root <- xmlRoot(xmlInternalTreeParse("tcxfiles//20150524-dwg-b.tcx"))
ns <- c(ns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2")

print("trees parsed")
print(now())


# so lots of this eventually gets put into a loop over files
# takes a long time for these 100-milers we're playing with

# ktrack <- xmlToDataFrame(nodes <- getNodeSet(kgw_lr_root, "//ns:Track", ns))
# this throws an error and was just done for exploration anyway

kactivity <- xmlToDataFrame(nodes <- getNodeSet(kgw_lr_root, "//ns:Activity", ns), stringsAsFactors = FALSE)
print("kactivity")
print(now())

# one obs of [laps + 2] variables
# Id = start time.
# one var per lap containing lap data squinched together down to the trackpoint
# one obs containing the creator info squinched
# var names are a mess


# klap <- xmlToDataFrame(nodes <- getNodeSet(kgw_lr_root, "//ns:Lap", ns))
klap <- Fun2(kgw_lr_root)
print("klap")
print(now())

# one obs per lap
# TotalTimeSeconds
# DistanceMeters
# MaximumSpeed
# Calories
# AverageHeartRateBpm
# MaximumHeartRateBpm
# Intensity ("Active"/"Resting")
# Cadence
# TriggerMethod (Manual, Distance, Location, Time, HeartRate)
# Track
# Extensions (below, all crammed together)
# name ("Lap")
# StartTime (yay!)

klap_ex <- xmlToDataFrame(nodes <- getNodeSet(kgw_lr_root, "//ns:Lap/ns:Extensions", ns), stringsAsFactors = FALSE)
print("klap_ex")
print(now())

# Extensions (MaxBikeCadence, AvgSpeed, AvgWatts, MaxWatts)
# (not sure we really need this but if you want 'em)

ktrackpoints <- xmlToDataFrame(nodes <- getNodeSet(kgw_lr_root, "//ns:Trackpoint", ns), stringsAsFactors = FALSE)
print("ktrackpoints")
print(now())

# goes away to the faeries for a long time on big files like this.
# start looking for efficiencies.
# Time (comes in as chr, like all these, we'll have to parse later)
# Position (lat lon crammed together, as they're being pulled out of child tags)
# DistanceMeters (total distance for ride as of this moment)
# Cadence
# Extensions (because for now only Watts is the extension, that's all we get.
# Probably safer to extract Watts by name and append later in case we get other
# extended instrumentation)

# ktp_ex <- xmlToDataFrame(nodes <- getNodeSet(kgw_lr_root, "//ns:Trackpoint/ns:Extensions", ns), stringsAsFactors = FALSE)
# like this. although it felt kind of lame.

kpositions <- xmlToDataFrame(nodes <- getNodeSet(kgw_lr_root, "//ns:Trackpoint/ns:Position", ns), stringsAsFactors = FALSE)
print("kpositions")
print(now())

#go get all the detail positions instead of trying to break apart random-length nos

print("Kate loaded")

dactivity <- xmlToDataFrame(nodes <- getNodeSet(dwg_lr_root, "//ns:Activity", ns), stringsAsFactors = FALSE)
dlap <- Fun2(dwg_lr_root)
dlap_ex <- xmlToDataFrame(nodes <- getNodeSet(dwg_lr_root, "//ns:Lap/ns:Extensions", ns), stringsAsFactors = FALSE)
dtrackpoints <- xmlToDataFrame(nodes <- getNodeSet(dwg_lr_root, "//ns:Trackpoint", ns), stringsAsFactors = FALSE)
dpositions <- xmlToDataFrame(nodes <- getNodeSet(dwg_lr_root, "//ns:Trackpoint/ns:Position", ns), stringsAsFactors = FALSE)

# that positions call gives us positions EXCEPT for nas which is kind of annoying.
# solved by joining parsed positions back to the trackpoints.

print("David loaded")

#now that we have the raw data, let's start tidying.

dpositions$Position <- paste(as.character(dpositions$LatitudeDegrees), as.character(dpositions$LongitudeDegrees), sep = "")
dtrackpoints <- left_join(dtrackpoints, dpositions, by = "Position")
rm(dpositions)
dtrackpoints <- select(dtrackpoints, -Position)
pointnames <- names(dtrackpoints)
names(dtrackpoints)[which(pointnames == "Extensions")] <- "Watts"

dactivity <- dactivity$Id

dlap <- select(dlap, -Intensity, -Calories, -name, -Track, -Extensions)
names(dlap_ex) <- c("MaxCadence", "AvgSpeed", "AvgWatts", "MaxWatts") 

# now file 2
kpositions$Position <- paste(as.character(kpositions$LatitudeDegrees), as.character(kpositions$LongitudeDegrees), sep = "")
ktrackpoints <- left_join(ktrackpoints, kpositions, by = "Position")
rm(kpositions)
ktrackpoints <- select(ktrackpoints, -Position)
pointnames <- names(ktrackpoints)
names(ktrackpoints)[which(pointnames == "Extensions")] <- "Watts"

kactivity <- kactivity$Id

klap <- select(klap, -Intensity, -Calories, -name, -Track, -Extensions)
names(klap_ex) <- c("MaxCadence", "AvgSpeed", "AvgWatts", "MaxWatts") 
# risky, depends on col order, calculate instead

dlap <- bind_cols(dlap, dlap_ex)
klap <- bind_cols(klap, klap_ex)
rm(dlap_ex)
rm(klap_ex)
rm(nodes)

print("Tidying Pass One Done")