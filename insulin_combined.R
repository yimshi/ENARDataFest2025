# Load necessary libraries
library(tidyverse)
library(haven)

# Define the dataset metadata
insulin_files <- data.frame(
  Data_File_Name = c("GLU_E", "LAB10AM", "GLU_D", "L10AM_C", "L10AM_B", "GLU_F", "GLU_G",
                     "INS_H", "INS_I", "INS_J", "INS_L"),
  Begin_Year = c(2007, 1999, 2005, 2003, 2001, 2009, 2011, 2013, 2015, 2017, 2021),
  End_Year = c(2008, 2000, 2006, 2004, 2002, 2010, 2012, 2014, 2016, 2018, 2023)
)

# Base URL for NHANES data files
base_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/"

# Initialize an empty list to store data frames
insulin_data_list <- list()

# Loop through the metadata to download, read, and process the files
for (i in 1:nrow(insulin_files)) {
  # Construct the file URL
  file_url <- paste0(base_url, insulin_files$Begin_Year[i], "/DataFiles/", insulin_files$Data_File_Name[i], ".xpt")
  
  # Define a temporary file path
  temp_file <- tempfile(fileext = ".xpt")
  
  # Try downloading the file
  tryCatch({
    download.file(file_url, temp_file, mode = "wb")
    # Read the SAS transport file
    temp_data <- read_xpt(temp_file)
    # Add survey year columns
    temp_data$Begin_Year <- insulin_files$Begin_Year[i]
    temp_data$End_Year <- insulin_files$End_Year[i]
    # Rename 'LBXINSI' to 'LBDINSI' if it exists in the dataset
    if ("LBXINSI" %in% names(temp_data)) {
      temp_data <- temp_data %>% rename(LBDINSI = LBXINSI)
    }
    # Append to list
    insulin_data_list[[i]] <- temp_data
  }, error = function(e) {
    message(paste("Error downloading:", file_url))
  })
}

# Combine all downloaded datasets
insulin_combined <- bind_rows(insulin_data_list)

# View the combined dataset
print(head(insulin_combined))

# Optionally, save the combined dataset
write_csv(insulin_combined, "G:/Google Downloads/nhanes-bp-2023-main/NHANES_data/insulin_combined.csv")
