# Load required packages
library(tidyverse)
library(readxl)
library(lubridate)

# Define function to process data
process_data <- function(input, output, directory){
  processed_data <- readr::read_csv(input) |>
    dplyr::select(
      `individual-local-identifier` = `Collar ID`,
      `timestamp` = `Acq. Time [UTC]`,
      `location-latitude` = `Latitude[deg]`,
      `location-longitude` = `Longitude[deg]`,
      altitude = `Altitude[m]`,
      `sensor-type` = `Fix Type`,
      DOP = any_of(c("DOP", "Dop"))
    ) |>
    na.omit() |>
    dplyr::mutate(
      `tag-local-identifier` = "VHF", 
      `study-timestamp` = ymd_hms(timestamp, tz = "UTC"),
      `study-timestamp` = lubridate::with_tz(`study-timestamp`, tzone = "America/Denver"),
      `study-name`= "MPG Bears",
      `study-timezone` = "Mountain Time"
    )
  
  # Create the full output file path
  output_path <- file.path(directory, output)
  
  # Write the processed data to the specified output directory
  readr::write_csv(processed_data, output_path)
}

file_list <- list.files("Raw Data/", pattern = ".csv", full.names=TRUE, recursive=TRUE)

# Loop over files and call the process_data function for each file
for (file in file_list){
  cat(file, "\n")
  process_data(input = file, output = basename(file), directory = "Derived Data")
}

