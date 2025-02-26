# presence-absence
code for my presence absence data


setwd("~/Documents/Canterbury/R data sets")
library(tidyverse)
read_csv("bird ID.csv")

library(dplyr)

# Load the dataset (make sure the file is in your working directory or specify the full path)
bird_data <- read.csv("Bird ID.csv", stringsAsFactors = FALSE)

# View the first few rows to check column names
head(bird_data)

# Check unique values in the age marking column to confirm chick labels
unique(bird_data$Age_marking)
library(dplyr)

# Filter and create a dataframe for chicks
chick_data <- bird_data %>% filter(Age_marking == "Chick")

# View the first few rows
head(chick_data)
colnames(chick_data)
names(chick_data)
library(dplyr)

chick_filtered <- chick_data %>%
  select(`Bird.ID`, `DateTime_marking`, `Weight_marking`, `Status_marking`)

# View the new dataframe
head(chick_filtered)



library(lubridate)

# Convert DateTime column to proper datetime format
monitoring_data$DateTime <- mdy_hms(monitoring_data$DateTime)

# Extract only the date in d/m/y format
monitoring_data$Date <- format(monitoring_data$DateTime, "%d/%m/%Y")

# Check results
head(monitoring_data$Date)

#create wide data from the bird id and monitoring data date frames

# Ensure Date column is properly formatted
monitoring_filtered_data$Date <- dmy(monitoring_filtered_data$Date)

# Extract all Bird.IDs from chick_filtered_dmy (including duplicates)
bird_ids <- chick_filtered_dmy$Bird.ID  

# Extract unique dates from monitoring_filtered_data and convert to character
unique_dates <- as.character(unique(na.omit(monitoring_data$Date)))

# Create a dataframe with Bird.ID as rows
wide_data <- data.frame(Bird.ID = bird_ids)

# Add unique dates as columns, initializing with NA (change to 0 if binary tracking is needed)
for (date in unique_dates) {
  wide_data[[date]] <- NA  # Use 0 if tracking presence/absence
}

# Check the structure
str
head(wide_data)

monitoring_filtered_data <- monitoring_data[, c("Adult.1", "Adult.2", "Date")]

# turning the monitoring filtered data into a wide data set

# Ensure Date column is correctly formatted (optional)
monitoring_filtered_data$Date <- as.character(monitoring_filtered_data$Date)  # Convert Date to character

# Create a unique identifier for each instance of the same date
monitoring_filtered_data <- monitoring_filtered_data %>%
  group_by(Date) %>%
  mutate(Date_uniq = paste(Date, row_number(), sep = "_")) %>%
  ungroup()

# Reshape the data: Gather Adult.1 and Adult.2 into a long format, so they appear as rows
long_data <- monitoring_filtered_data %>%
  pivot_longer(cols = c(Adult.1, Adult.2), names_to = "Adult_Type", values_to = "Bird_ID") %>%
  filter(!is.na(Bird_ID))  # Remove rows where Bird_ID is NA

# Reshape to wide format: Dates as columns (unique names), and Adult.1/Adult.2 as rows
wide_data <- long_data %>%
  pivot_wider(names_from = Date_uniq, values_from = Bird_ID, values_fill = list(Bird_ID = NA))  # Keep NA if no bird ID

# Remove the Date column from column 1
wide_data <- wide_data %>% select(-`Date`)

# View the reshaped data
head(wide_data)



