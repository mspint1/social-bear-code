
# List all .rds files in directory
file_list <- list.files("Derived Data/", pattern = "Positions_HR_model.rds", full.names = TRUE, recursive = TRUE)

process_custom_rds <- function(file_path) {
  message("Processing: ", file_path)
  
  # create an empty data.frame to put data into
  empty_row <- data.frame(
    File = file_path,
    Name = NA_character_,
    AKDE_Estimate_km2 = NA_real_,
    AKDE_LowerCI_km2 = NA_real_,
    AKDE_UpperCI_km2 = NA_real_,
    Error = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Read RDS
  if (!file.exists(file_path)) {
    empty_row$Error <- "File does not exist"
    return(empty_row)
  }
  
  loaded_data <- tryCatch(readRDS(file_path), error = function(e) {
    empty_row$Error <- paste("readRDS error:", e$message)
    return(empty_row)
  })
  
  # Load akde result
  akde_result <- if (inherits(loaded_data, "UD") || inherits(loaded_data, "KDE")) {
    loaded_data
  } else if ("akde_result" %in% names(loaded_data)) {
    loaded_data$akde_result
  } else {
    empty_row$Error <- "No akde_result found"
    return(empty_row)
  }
  
  # Gather range summary
  range_summary <- tryCatch(summary(akde_result), error = function(e) {
    empty_row$Error <- paste("summary() failed:", e$message)
    return(empty_row)
  })
  
  if (!("CI" %in% names(range_summary))) {
    empty_row$Error <- "CI structure missing"
    return(empty_row)
  }
  
  # Get confidence interval
  ci <- range_summary$CI
  if (!"area (square kilometers)" %in% rownames(ci)) {
    empty_row$Error <- "Missing 'area (square kilometers)' row"
    return(empty_row)
  }
  
  identity <- tryCatch(akde_result@info$identity, error = function(e) NA_character_)
  
  # fill created dataframe
  data.frame(
    File = file_path,
    Name = identity,
    AKDE_Estimate_km2 = ci["area (square kilometers)", "est"],
    AKDE_LowerCI_km2 = ci["area (square kilometers)", "low"],
    AKDE_UpperCI_km2 = ci["area (square kilometers)", "high"],
    Error = NA_character_,
    stringsAsFactors = FALSE
  )
}

# Function to get the homerange size
home_ranges <- do.call(rbind, lapply(file_list, function(file) {
  tryCatch({
    process_custom_rds(file)
  }, error = function(e) {
    message("Error processing file: ", file, " - ", e$message)
    return(data.frame(File = file, Error = e$message))
  })
}))


# Save to CSV
write.csv(home_ranges, "homerange_size.csv", row.names = FALSE)

################## Include Error bears which have units in Hectares or Square Meters for no reason #################

Blue2_2 <- read_rds("Derived Data/Blue2/2024_Blue2_2_PositionsBreeding_HR_model.rds")
Blue_summary <- summary(Blue2_2$akde_result)
Blue_area_km2 <- Blue_summary$CI["area (hectares)", "est"] / 100
Blue_area_low_km2 <- Blue_summary$CI["area (hectares)", "low"] / 100
Blue_area_high_km2 <- Blue_summary$CI["area (hectares)", "high"] / 100
list(Blue_area_km2, Blue_area_low_km2, Blue_area_high_km2)

Louise <- read_rds("Derived Data/Louise/2024_Louise_PositionsNonbreeding_HR_model.rds")
Louise_summary <- summary(Louise$akde_result)
Louise_area_km2 <- Louise_summary$CI["area (square meters)", "est"] / 1e6
Louise_area_low_km2 <- Louise_summary$CI["area (square meters)", "low"] / 1e6
Louise_area_high_km2 <- Louise_summary$CI["area (square meters)", "high"] / 1e6
list(Louise_area_km2, Louise_area_low_km2, Louise_area_high_km2)

Skunk <- read_rds("Derived Data/Skunk/2024_Skunk_PositionsNonbreeding_HR_model.rds")
Skunk_summary <- summary(Skunk$akde_result)
Skunk_area_km2 <- Skunk_summary$CI["area (square meters)", "est"] / 1e6
Skunk_area_low_km2 <- Skunk_summary$CI["area (square meters)", "low"] / 1e6
Skunk_area_high_km2 <- Skunk_summary$CI["area (square meters)", "high"] / 1e6
list(Skunk_area_km2, Skunk_area_low_km2, Skunk_area_high_km2)

SplitEar_2 <- read_rds("Derived Data/Split_Ear/2024_SplitEar_2_PositionsNonbreeding_HR_model.rds")
Split_summary <- summary(SplitEar_2$akde_result)
Split_area_km2 <- Split_summary$CI["area (hectares)", "est"] / 100
Split_area_low_km2 <- Split_summary$CI["area (hectares)", "low"] / 100
Split_area_high_km2 <- Split_summary$CI["area (hectares)", "high"] / 100
list(Split_area_km2, Split_area_low_km2, Split_area_high_km2)

