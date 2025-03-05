# Load necessary library
library(dplyr)

# Function to combine data frames from a list and save to a CSV
combine_and_save <- function(ISRaD_extra, output_file) {
  # Combine all data frames in the list, handling missing columns with NA
  combined_df <- bind_rows(ISRaD_extra)
  
  # Write the combined data frame to a CSV file
  write.csv(combined_df, output_file, row.names = FALSE)
}

# Example: Assuming ISRaD_extra is already defined as a list of data frames
# ISRaD_extra should be a list containing your data frames like so:
# ISRaD_extra <- list(metadata, site, profile, flux, layer, interstitial, fraction, incubation)

# Call the function with ISRaD_extra and the desired output file name
combine_and_save(ISRaD_extra, "combined_data.csv")
