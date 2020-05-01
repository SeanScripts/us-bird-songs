# JSON data is from xeno-canto
# https://www.xeno-canto.org/
# RStudio

#library(rstudioapi)
library(jsonlite)
library(lubridate)
library(stringr)
library(ggplot2)
library(sqldf)

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

base_url <- "https://www.xeno-canto.org/api/2/recordings?query=cnt:united_states"

birdjson <- jsonlite::fromJSON(base_url)

# Metadata
numRecordings <- birdjson$numRecordings #50394
numSpecies <- birdjson$numSpecies #803
numPages <- birdjson$numPages #101

# Add all pages (this may take a while.)
recordings <- NULL
for (page in 1:numPages) {
  page_url <- paste(paste(base_url, "&page=", sep=""), page, sep="")
  print(page_url)
  pagejson = jsonlite::fromJSON(page_url)
  recordings <- rbind(recordings, pagejson$recordings[,-c(6,7,8,11,13,16,17,20,21,22,24,25,26)])
}

# Interestingly, 3 more than the total that was listed
# I wonder what the extra 3 rows are?

# The also column may be useful in some ways, but for now I will remove it
recordings <- recordings[,-23]

# Sanity check
sum(duplicated(recordings))

# Genus: 374
length(unique(recordings$gen))
# Species: 675 (some overlap between genus)
length(unique(recordings$sp))
# Genus + species: 803
length(unique(paste(recordings$gen, recordings$sp)))
# Genus + species + subspecies: 1653
length(unique(paste(paste(recordings$gen, recordings$sp), recordings$ssp)))

# Remove unidentified entries
recordings <- recordings[-which(recordings$gen == "Mystery"),]
# 49527 left

# Check recording lengths
# Are any of them over 1 hour? I really hope not
audio_str_len <- sapply(recordings$length, str_length)
max(audio_str_len) # Seriously?
which.max(audio_str_len) # A two hour bird recording? Who does that?
which(audio_str_len > 4) # These are simply too long.

# Remove all recordings over 10 minutes (probably could remove even shorter ones too)
recordings <- recordings[-which(audio_str_len > 4),]
# 49416 left

# Convert lengths to seconds
recordings$rec_length <- as.numeric(lubridate::ms(recordings$length))
recordings <- recordings[,-12] # Remove old column

# Gather some more metadata
# Total time
totalLength <- sum(recordings$rec_length)
print(totalLength/3600) # 757 Hours...

# Average time
averageLength <- totalLength/nrow(recordings)
print(averageLength) # That's a bit long.

# Check distribution
hist(recordings$rec_length)
bp <- boxplot(recordings$rec_length)
# Safe to say these long end ones can be removed.
maxlen <- bp$stats[5]
print(maxlen)
length(which(recordings$rec_length > maxlen))
# Remove audios over 2 minutes 23 seconds (outliers)
recordings <- recordings[-which(recordings$rec_length > maxlen),]
# 45566 left

# Check distributions of other variables

# Type
# There are so many labels for type here (2338) that this is not really useful
length(unique(recordings$type))
barplot(log(table(recordings$type)))
head(unique(recordings$type), 12)
# No wonder it's not very useful. Some are very general and others are very specific.

# Quality
# Rated from A to E, plus some with no score given yet
barplot(table(recordings$q))

# First, remove the very low quality recordings, and the ones with no score yet
recordings <- recordings[-which(recordings$q == "D"),]
recordings <- recordings[-which(recordings$q == "E"),]
recordings <- recordings[-which(recordings$q == "no score"),]
pie(table(recordings$q))
# 41561 left

# Convert long and lat to numeric (some nulls)
recordings$lng <- as.numeric(recordings$lng)
recordings$lat <- as.numeric(recordings$lat)

# Remove the rows with nulls when the long and lat were unknown
recordings <- recordings[complete.cases(recordings),]
# 40857 left

# Commas in the type and file name columns are annoying
recordings <- recordings[,-8]
recordings <- recordings[,-9]

# Split into three versions:
#  Best quality (A)
#  Medium quality (A + B)
#  General quality (A + B + C)

# Best: 14967 observations
length(which(recordings$q == "A"))
recordings_best <- recordings[which(recordings$q == "A"),]
# Medium: 34059 observations
length(which(recordings$q == "A" | recordings$q == "B"))
recordings_medium <- recordings[which(recordings$q == "A" | recordings$q == "B"),]
# General: 40857 observations

# Checking the best quality set
hist(recordings_best$rec_length)
boxplot(recordings_best$rec_length)
# Could further remove outliers on recording length but I don't think it's necessary
totalLengthBest <- sum(recordings_best$rec_length)
print(totalLengthBest/3600) #~190 Hours. Still a lot of audio data.
# 693 species now
length(unique(paste(recordings_best$gen, recordings_best$sp)))
recordings_best$species <- paste(recordings_best$gen, recordings_best$sp)
barplot(table(recordings_best$species))
# Could arbitrarily choose a threshold here... 
# 700 species still seems like a bit too many to work with.
# Maybe I'll look at just Texas first.

# First, continental us
recordings_best <- recordings_best[which(recordings_best$lat < 50 & recordings_best$lng > -129 & recordings_best$lng < -60),]
# 13885 left
# and 608 species
length(unique(paste(recordings_best$gen, recordings_best$sp)))

states <- data.frame(state=tolower(state.name), value=runif(50), stringsAsFactors = FALSE)
us <- ggplot2::map_data("state")

us_map <- ggplot() + geom_polygon(data=us, aes(x=long, y=lat, group=group), color="black", fill="white") + coord_map()
us_map + geom_point(aes(x=recordings_best$lng, y=recordings_best$lat), color="red")
# Looks like some states are way more popular than others.

# Just check Texas only first
# Birds don't care about state boundaries,
# so I'm just going to look at the birds "near" Texas.
center_x <- -99
center_y <- 31
radius <- 8
texas_only <- recordings_best[which((recordings_best$lng-center_x)**2 + (recordings_best$lat-center_y)**2 < radius**2),]
# 991 left
us_map + geom_point(aes(x=texas_only$lng, y=texas_only$lat), color="red")
# There are a few outside Texas still, but this is pretty good

# Check total length and number of species left
totalLengthTexas <- sum(texas_only$rec_length)
print(totalLengthTexas/3600) #~12 Hours. Much more manageable.
# 237 species now
length(unique(texas_only$species))
barplot(table(texas_only$species))

# Total recording times for each species
texas_times <- sqldf("SELECT species, en, COUNT(id) AS num_recs, SUM(rec_length) AS total_length FROM texas_only GROUP BY species ORDER BY total_length DESC")
# Total recording length for each species in Texas area
barplot(texas_times$total_length)
# Nice distribution! Now how many classes to remove...
# Surely classes with less than five minutes of data could be removed, but that's an arbitrary bound
# I'd say classes with fewer than five recordings could have enough bias to not be worth considering

texas_classes <- texas_times[which(texas_times$num_recs >= 5 & texas_times$total_length >= 5*60),]
# This leaves 45 classes
barplot(texas_classes$total_length)
sum(texas_classes$total_length)/3600 # 6.8 hours now

texas_final <- sqldf("SELECT * FROM texas_only WHERE species IN (SELECT species FROM texas_classes)")
unique(texas_final$species)
# 45 species, 509 recordings, 6.8 hours

# Save the dataframes to CSV so I don't have to download and reprocess them
write.csv(recordings_best, "bird_recordings_best.csv", row.names=FALSE)
write.csv(recordings_medium, "bird_recordings_medium.csv", row.names=FALSE)
write.csv(recordings, "bird_recordings_general.csv", row.names=FALSE)

write.csv(texas_only, "bird_recordings_texas.csv", row.names=FALSE)
write.csv(texas_final, "bird_recordings_texas_final.csv", row.names=FALSE)

for (i in 1:nrow(texas_final)) {
  id <- texas_final$id[i]
  sound_file_url <- paste("https:", texas_final$file[i], sep="")
  print(paste("Downloading file", i))
  download.file(sound_file_url, paste(paste("audio/", id, sep=""), ".mp3", sep=""), mode="wb")
}

