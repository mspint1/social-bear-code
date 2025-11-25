
# Load required packages
library(ctmm)
library(sf)
library(ggplot2)

# Define the working directory to search for RDS files
parent_directory <- "Derived Data/"

# List all RDS files in directory
rds_files <- list.files(path = parent_directory, pattern = "PositionsPositions_HR_model.rds", full.names = TRUE, recursive = TRUE)

# Loop through each RDS file
for (rds_file in rds_files) {
 
  akde_result <- readRDS(rds_file)
  
  
  akde_sf <- as.sf(akde_result$akde_result, level.UD = 0.50) # Change level.UD = 0.95 for 95 percent UD
  tele_sf <- as.sf(akde_result$cleaned_telemetry)
  
  # Plot for verification
  plot_title <- paste("Homerange for", basename(rds_file))  
  ggplot(data = akde_sf) +
    geom_sf(aes(fill = name), alpha = 0.5) +  # Fill by individual ID
    theme_minimal() +
    labs(title = plot_title, fill = "Bear ID")
  
  # Create output folder 
  relative_path <- gsub(parent_directory, "", dirname(rds_file))
  output_folder <- file.path("data/", relative_path) 
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE) 
  
  akde_shapefile <- paste0("data/", tools::file_path_sans_ext(basename(rds_file)), "_akde.shp")
  tele_shapefile <- paste0("data/", tools::file_path_sans_ext(basename(rds_file)), "_tele.shp")
  
  # Export AKDE home range to shapefile
  st_write(akde_sf, akde_shapefile, driver = "ESRI Shapefile", delete_layer = TRUE)
  
  # Export telemetry data to shapefile
  st_write(tele_sf, tele_shapefile, driver = "ESRI Shapefile", delete_layer = TRUE)
}

