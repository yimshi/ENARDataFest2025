# Load necessary libraries
library(tidyverse)
library(haven)

# Define the dataset metadata
urine_metal_files <- data.frame(
  Data_File_Name = c("UHM_E", "UHM_D", "L06HM_C", "L06HM_B", "UHM_F", 
                     "UHM_G", "UM_H", "UM_I", "UM_J", "P_UM"),
  Begin_Year = c(2007, 2005, 2003, 2001, 2009, 
                 2011, 2013, 2015, 2017, 2017),
  End_Year = c(2008, 2006, 2004, 2002, 2010, 
               2012, 2014, 2016, 2018, 2020)
)

# Base URL for NHANES data files
base_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/"

# Initialize an empty list to store data frames
urine_metal_data_list <- list()

# Loop through the metadata to download, read, and process the files
for (i in 1:nrow(urine_metal_files)) {
  # Construct the file URL
  file_url <- paste0(base_url, urine_metal_files$Begin_Year[i], "/DataFiles/", urine_metal_files$Data_File_Name[i], ".xpt")
  
  # Define a temporary file path
  temp_file <- tempfile(fileext = ".xpt")
  
  # Try downloading the file
  tryCatch({
    download.file(file_url, temp_file, mode = "wb")
    # Read the SAS transport file
    temp_data <- read_xpt(temp_file)
    # Add survey year columns
    temp_data$Begin_Year <- urine_metal_files$Begin_Year[i]
    temp_data$End_Year <- urine_metal_files$End_Year[i]
    # Append to list
    urine_metal_data_list[[i]] <- temp_data
  }, error = function(e) {
    message(paste("Error downloading:", file_url))
  })
}

# Combine all downloaded datasets
urine_metal_combined <- bind_rows(urine_metal_data_list)

# View the combined dataset
print(head(urine_metal_combined))

# Optionally, save the combined dataset
write_csv(urine_metal_combined, "G:/Google Downloads/nhanes-bp-2023-main/NHANES_data/urine_metal_combined.csv")
