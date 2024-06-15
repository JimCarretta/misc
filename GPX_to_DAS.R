# read an external GPX file and create a DAS-based text file of time, date, and position data

# useful if you have a GPS connectivity issue with WinCruz or VisCruz software,
# such that your DAS output files lack GPS data. If you have access to an independent
# GPX file from the same time period, you can use this to re-create the missing position
# data for DAS.
#

library(xml2)

rm(list=ls())

# output file defined
output_path <- "c:/carretta/DAS_rescue.txt"

# Load the GPX file
gpx_file <- "c:/carretta/Track_2024-06-14.gpx"

# Read the GPX file
gpx_data <- read_xml(gpx_file)

# Get all namespace definitions from the GPX file
ns <- xml_ns(gpx_data)

# Define a prefix for the default namespace (xmlns)
ns_def <- c(gp = ns[["d1"]])

# extract points using namespace

track_points <- xml_find_all(gpx_data, ".//gp:trkpt", ns_def)

# Initialize vectors to store the extracted data
latitudes <- numeric(length(track_points))
longitudes <- numeric(length(track_points))
times <- character(length(track_points))

# Loop through each track point and extract the data
for (i in seq_along(track_points)) {
  latitudes[i] <- as.numeric(xml_attr(track_points[i], "lat"))
  longitudes[i] <- as.numeric(xml_attr(track_points[i], "lon"))
  times[i] <- xml_text(xml_find_first(track_points[i], ".//gp:time", ns_def))
}

# Combine the data into a data frame
gpx_df <- data.frame(
  latitude = latitudes,
  longitude = longitudes,
  time = as.POSIXct(times, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
)

# View the first few rows of the data frame
head(gpx_df)

# convert the data frame to a DAS location and date format

# format required in columns 6 through 39 of DAS file: 073242 062918 N44:12.09 W129:00.81
# hhmmss mmddyy latitude longitude

# time conversion
time.DAS <- paste(substr(gpx_df$time, 12, 13),
                         substr(gpx_df$time, 15, 16),
                         substr(gpx_df$time, 18, 19), sep="")

date.DAS <- paste(substr(gpx_df$time, 9, 10),
                         substr(gpx_df$time, 6, 7),
                         substr(gpx_df$time, 3, 4), sep="")

lat.DAS <- paste("N", substr(gpx_df$latitude, 1, 2), ":",
                        as.numeric(substr(gpx_df$latitude, 3, 8))*60, sep="")


long.DAS <- paste("W", substr(gpx_df$longitude, 2, 4), ":",
                        as.numeric(substr(gpx_df$longitude, 5, 9))*60, sep="")

# truncate to DAS format

lat.DAS <- substr(lat.DAS, 1, 9)
long.DAS <- substr(long.DAS, 1, 10)

df.out <- cbind.data.frame(gpx_df, time.DAS, date.DAS, lat.DAS, long.DAS)

head(df.out)

# concatenate time + date + lat + long to DAS fixed-width format

DAS.fields <- cbind.data.frame(time.DAS, date.DAS, lat.DAS, long.DAS)

head(DAS.fields)

output <- rbind(paste(DAS.fields$time.DAS, DAS.fields$date.DAS, DAS.fields$lat.DAS, DAS.fields$long.DAS))

# Write the formatted lines to a fixed-width file
writeLines(output, con = output_path)


