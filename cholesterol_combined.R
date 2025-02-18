# Load necessary libraries
library(tidyverse)
library(haven)

# Define the dataset metadata
cholesterol_files <- data.frame(
  Data_File_Name = c("BIOPRO_E", "TCHOL_E", "TRIGLY_E", "HDL_E", "Lab13", "LAB13AM",
                     "LAB18", "l13_b", "l13_c", "TCHOL_D", "HDL_D", "TRIGLY_D",
                     "L13AM_C", "L40_C", "L13AM_B", "L40_B", "TCHOL_F", "HDL_F",
                     "TRIGLY_F", "TCHOL_G", "HDL_G", "TRIGLY_G", "TCHOL_H",
                     "HDL_H", "TRIGLY_H", "BIOPRO_I", "HDL_I", "TCHOL_I",
                     "TRIGLY_I", "TCHOL_J", "HDL_J", "TRIGLY_J", "P_HDL",
                     "P_TCHOL", "P_BIOPRO", "HDL_L", "TCHOL_L"),
  Begin_Year = c(2007, 2007, 2007, 2007, 1999, 1999, 1999, 2001, 2003, 2005, 2005, 2005,
                 2003, 2003, 2001, 2001, 2009, 2009, 2009, 2011, 2011, 2011, 2013,
                 2013, 2013, 2015, 2015, 2015, 2015, 2017, 2017, 2017, 2017,
                 2017, 2017, 2021, 2021)
)

# Base URL for NHANES data files
base_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/"

# Initialize an empty list to store data frames
cholesterol_data_list <- list()

# Loop through the metadata to download, read, and process the files
for (i in 1:nrow(cholesterol_files)) {
  # Construct the file URL
  file_url <- paste0(base_url, cholesterol_files$Begin_Year[i], "/DataFiles/", cholesterol_files$Data_File_Name[i], ".xpt")
  
  # Define a temporary file path
  temp_file <- tempfile(fileext = ".xpt")
  
  # Try downloading the file
  tryCatch({
    download.file(file_url, temp_file, mode = "wb")
    # Read the SAS transport file
    temp_data <- read_xpt(temp_file)
    # Add survey year column
    temp_data$Survey_Year <- cholesterol_files$Begin_Year[i]
    # Append to list
    cholesterol_data_list[[i]] <- temp_data
  }, error = function(e) {
    message(paste("Error downloading:", file_url))
  })
}

# Combine all downloaded datasets
cholesterol_combined <- bind_rows(cholesterol_data_list)

# View the combined dataset
print(head(cholesterol_combined))

# Optionally, save the combined dataset
write_csv(cholesterol_combined, "G:/Google Downloads/nhanes-bp-2023-main/NHANES_data/cholesterol_combined.csv")
