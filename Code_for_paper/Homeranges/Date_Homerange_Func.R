# Load required packages
library(ctmm)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# Season Dates:
  #Spring: 04-03, 05-23
  #Breeding: 05-24, 08-07
  #Fall: 08-08, 10-29

# Homerange function by data
process_telemetry_data <- function(file_path, plot_data = TRUE, result_env = new.env()) {
  cat("Processing file:", file_path, "\n")
  
  # verify existence of file (best for mistaken titles)
  if (!file.exists(file_path)) {
    stop("File does not exist.")
  }
  
  outfile <- str_replace(file_path, ".csv", "_Breeding_HR_model.rds") # rename to whatever season you want
  
  # Check to see if the file already exists
  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
  
  if (file.exists(outfile)) {
    cat("Output file already exists. Skipping:", outfile, "\n")
    return(outfile)
  }
  
  # Read the telemetry data 
  telemetry_data <- tryCatch({
    cat("Reading CSV file...\n")
    read_csv(file_path)
  }, error = function(e) {
    stop("Error reading the CSV file: ", e$message)
  })
  
  # Convert to a data frame
  telemetry_data <- as.data.frame(telemetry_data)
  
  # Turn timestamp into as.date format
  telemetry_data$timestamp <- as.POSIXct(telemetry_data$timestamp, format = "%m/%d/%Y %H:%M")
  
  # Define the month and day range for filtering (change for different seasons)
  start_month_day <- "05-24"  
  end_month_day <- "08-07"    
  
  # Filter by month and day, regardless of year
  telemetry_data <- telemetry_data %>%
    filter(
      format(timestamp, "%m-%d") >= start_month_day &
        format(timestamp, "%m-%d") <= end_month_day
    )
  
  if (nrow(telemetry_data) == 0) {
    cat("No data remains after filtering by date range. Skipping:", file_path, "\n")
    return(NULL)
  }
  
  # Convert to telemetry object
  telemetry_obj <- tryCatch({
    cat("Converting to telemetry object...\n")
    as.telemetry(telemetry_data, projection = "EPSG:6514")
  }, error = function(e) {
    stop("Error converting to telemetry object: ", e$message)
  })
  
  # Identify and remove outliers
  outliers <- tryCatch({
    cat("Identifying outliers...\n")
    outlie(telemetry_obj, plot = plot_data)
  }, error = function(e) {
    stop("Error identifying outliers: ", e$message)
  })
  
  # filter out speed outliers
  bad_data <- outliers$speed > 0.112
  cleaned_telemetry <- telemetry_obj[!bad_data, ]
  
  outliers_cleaned <- tryCatch({
    cat("Rechecking outliers after cleaning...\n")
    outlie(cleaned_telemetry)
  }, error = function(e) {
    stop("Error rechecking outliers: ", e$message)
  })
  
  # Estimate model parameters
  guess <- tryCatch({
    cat("Estimating initial model parameters...\n")
    ctmm.guess(cleaned_telemetry, interactive = FALSE)
  }, error = function(e) {
    stop("Error estimating initial model parameters: ", e$message)
  })
  
  # Fit the model
  fit <- tryCatch({
    cat("Fitting the model...\n")
    ctmm.fit(data = cleaned_telemetry, CTMM = guess, method = "pHREML")
  }, error = function(e) {
    stop("Error fitting the model: ", e$message)
  })
  
  # Perform AKDE (autocorrelated kernel density estimation)
  akde_result <- tryCatch({
    cat("Performing AKDE...\n")
    akde(cleaned_telemetry, fit, weights = FALSE)
  }, error = function(e) {
    stop("Error performing AKDE: ", e$message)
  })
  
  # Determine the extent of the estimated home range (0.95 or 0.5 depending on if core or not.)
  extent_result <- tryCatch({
    cat("Determining the extent of the home range...\n")
    extent(list(akde_result), level = 0.95)
  }, error = function(e) {
    stop("Error determining the extent of the home range: ", e$message)
  })
  
  # Plot the telemetry data and the AKDE for verification
  if (plot_data) {
    tryCatch({
      cat("Plotting the results...\n")
      plot(cleaned_telemetry, akde_result, xlim = extent_result$x, ylim = extent_result$y)
    }, error = function(e) {
      stop("Error plotting the results: ", e$message)
    })
  }
  
  # Save the results to an RDS file
  tryCatch({
    list(
      guess = guess,
      fit = fit,
      akde_result = akde_result,
      extent_result = extent_result
    ) |> write_rds(outfile)
    
    cat("Successfully processed and saved:", outfile, "\n")
  }, error = function(e) {
    stop("Error saving results: ", e$message)
  })
  
  return(outfile)
}

# Get list of CSV files
file_list <- list.files("Derived Data/", pattern = "Positions.csv", full.names = TRUE, recursive = TRUE)

# Loop over files and call the function
for (file in file_list) {
  tryCatch({
    process_telemetry_data(file_path = file, plot_data = TRUE)
  }, error = function(e) {
    cat("Error processing file:", file, "\n", e$message, "\n")
  })
}
