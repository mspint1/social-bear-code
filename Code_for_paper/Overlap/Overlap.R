# Load required packages
library(ctmm)
library(dplyr)
library(readr)
library(tibble)
library(tidyr)  


# list files
get_rds_files <- list.files("Derived Data/", pattern = "Positions_HR_model.rds", full.names = TRUE, recursive = TRUE)

# Load all bear data RDS files
bear_fits <- lapply(rds_files, function(file) {
  tryCatch({
    bear <- readr::read_rds(file)
    bear$fit
  }, error = function(e) {
    message("Error loading file: ", file)
    return(NULL)
  })
})

# Remove any NULL entries
bear_fits <- bear_fits[!sapply(bear_fits, is.null)]
bear_ids <- basename(get_rds_files[!sapply(bear_fits, is.null)])

# Create an empty matrix to save data into
overlap_matrix <- matrix(NA, nrow = length(bear_ids), ncol = length(bear_ids))
rownames(overlap_matrix) <- bear_ids
colnames(overlap_matrix) <- bear_ids

# Calculate overlap percentages for each pair of bears
for (i in 1:length(bear_ids)) {
  for (j in 1:length(bear_ids)) {
    if (i != j) {
      overlap_result <- ctmm::overlap(list(bear_fits[[i]], bear_fits[[j]]))
      overlap_est <- overlap_result$CI[,, "est"]  # Use the "est" column
      overlap_matrix[i, j] <- overlap_est[1, 2]  # Extract the overlap percentage
    } else {
      overlap_matrix[i, j] <- NA
    }
  }
}

# Convert the overlap matrix to a dataframe 
overlap_df <- as.data.frame(overlap_matrix)

# Convert row names to a column
overlap_df <- overlap_df |>
  rownames_to_column("Bear1")

# Pivot the data to long format
overlap_long <- overlap_df |>
  pivot_longer(-Bear1, names_to = "Bear2", values_to = "Overlap")

# Pivot the data back to wide format
overlap_wide <- overlap_long |>
  pivot_wider(names_from = Bear2, values_from = Overlap)
  spread(Bear2, Overlap)

# Replace NA values with 0
overlap_wide[is.na(overlap_wide)] <- 0

# Format significant figures for easier visualization
overlap_wide <- overlap_wide |>
  mutate(across(where(is.numeric), ~ formatC(.x, format = "f", digits = 4)))

# Write the overlap into a .csv file
readr::write_csv(overlap_wide, file = "Overlap_table.csv")
