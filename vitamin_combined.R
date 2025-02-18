# Load necessary libraries
library(tidyverse)
library(haven)

# Define the dataset metadata
vitamin_files <- data.frame(
  Data_File_Name = c("FASTQX_E", "L06VIT_C", "PH", "LAB06", "FASTQX_D", "VITAEC_D", "B12_D", "VIC_D",
                     "L06NB_C", "L43_C", "PH_C", "PH_B", "L06_B", "L06VIT_B", "L06VID_B", "VID_2_B",
                     "L06_2_B", "L06VID_C", "FASTQX_F", "VITB12_G", "FASTQX_G", "VITB12_H", "FASTQX_H",
                     "VID_B", "VID_C", "VID_D", "VID_E", "VID_F", "FASTQX_I", "VID_G", "VID_H",
                     "FASTQX_J", "VIC_J", "VID_I", "P_FASTQX", "VID_J", "FASTQX_L", "VID_L"),
  Begin_Year = c(2007, 2003, 1999, 1999, 2005, 2005, 2005, 2005,
                 2003, 2003, 2003, 2001, 2001, 2001, 2001, 2001,
                 2001, 2003, 2009, 2011, 2011, 2013, 2013, 2001,
                 2003, 2005, 2007, 2009, 2015, 2011, 2013, 2017,
                 2017, 2015, 2017, 2017, 2021, 2021),
  End_Year = c(2008, 2004, 2000, 2000, 2006, 2006, 2006, 2006,
               2004, 2004, 2004, 2002, 2002, 2002, 2002, 2002,
               2002, 2004, 2010, 2012, 2012, 2014, 2014, 2002,
               2004, 2006, 2008, 2010, 2016, 2012, 2014, 2018,
               2018, 2016, 2020, 2018, 2023, 2023)
)

# Base URL for NHANES data files
base_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/"

# Initialize an empty list to store data frames
vitamin_data_list <- list()

# Loop through the metadata to download, read, and process the files
for (i in 1:nrow(vitamin_files)) {
  # Construct the file URL
  file_url <- paste0(base_url, vitamin_files$Begin_Year[i], "/DataFiles/", vitamin_files$Data_File_Name[i], ".xpt")
  
  # Define a temporary file path
  temp_file <- tempfile(fileext = ".xpt")
  
  # Try downloading the file
  tryCatch({
    download.file(file_url, temp_file, mode = "wb")
    # Read the SAS transport file
    temp_data <- read_xpt(temp_file)
    # Add survey year columns
    temp_data$Begin_Year <- vitamin_files$Begin_Year[i]
    temp_data$End_Year <- vitamin_files$End_Year[i]
    # Append to list
    vitamin_data_list[[i]] <- temp_data
  }, error = function(e) {
    message(paste("Error downloading:", file_url))
  })
}

# Combine all downloaded datasets
vitamin_combined <- bind_rows(vitamin_data_list)

# View the combined dataset
print(head(vitamin_combined))

# Optionally, save the combined dataset
write_csv(vitamin_combined, "G:/Google Downloads/nhanes-bp-2023-main/NHANES_data/vitamin_combined.csv")

