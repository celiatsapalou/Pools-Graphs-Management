# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Define the function to load and clean the data
load_and_clean_data <- function(file_path) {
  df <- read_tsv(file_path, col_types = cols(
    Superpopulation = col_character(),
    SampleID = col_character(),
    cell = col_double(),
    color = col_character()
  ))
  
  # Print the structure of the dataframe for debugging
  print(paste("Loaded data from", file_path))
  print(str(df))
  
  expected_colnames <- c("Superpopulation", "SampleID", "cell", "color")
  missing_cols <- setdiff(expected_colnames, colnames(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  df <- df[, expected_colnames]  # Reorder columns to match expected order
  return(df)
}

# List all the tsv files in the folders and subfolders
base_dir <- "/Users/tsapalou/Downloads/PhD-2023/cross_analysis_2/pool1old/"
files <- list.files(base_dir, pattern = "sample_by_superpop_recalled_DATA_background.csv", recursive = TRUE, full.names = TRUE)

# Print the files to be processed for debugging
print("Files to be processed:")
print(files)

# Load and clean data
data_list <- lapply(files, load_and_clean_data)

# Print the length of data_list for debugging
print(paste("Number of data frames loaded:", length(data_list)))

# Combine the data
data_combined <- bind_rows(data_list)

# Group and summarise the data
data_combined <- data_combined %>%
  group_by(Superpopulation, SampleID) %>%
  summarise(cell = sum(cell, na.rm = TRUE), .groups = 'drop')

# Replace NA in Superpopulation with AMR
data_combined <- data_combined %>%
  mutate(Superpopulation = replace_na(Superpopulation, "AMR"))

# Reorder SampleID within each Superpopulation group by the number of cells
data_combined <- data_combined %>%
  group_by(Superpopulation) %>%
  arrange(Superpopulation, desc(cell), SampleID) %>%
  ungroup()

# Create a unified factor for SampleID to ensure continuous plotting
data_combined <- data_combined %>%
  mutate(SampleID = factor(SampleID, levels = unique(SampleID)))

# Define colors for each superpopulation
superpop_colors <- c("#ffd845", "#710027", "#018ead", "#2356A7", "#c44cfd")
names(superpop_colors) <- c("AFR", "AMR", "EAS", "EUR", "SAS")

# Plot for the combined data
plot_combined <- ggplot(data_combined, aes(x = SampleID, y = cell, fill = Superpopulation)) +
  geom_bar(stat = "identity", width = 0.9) +  # Adjust bar width
  scale_fill_manual(values = superpop_colors) +
  labs(title = "Cell Count by SampleID and Superpopulation - Combined Data",
       y = "Cell Counts") +
  ylim(0, 40) +  # Set the y-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10), # Reduced font size and steeper angle
        panel.grid.major.x = element_blank(),  # Remove gaps between superpopulations
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.ticks.x = element_blank())  # Hide the x-axis ticks for clarity

# Print the plot
print(plot_combined)

# Save the combined data
write.csv(data_combined, "/mnt/data/combined_data.csv", row.names = FALSE)

# Calculate the average number of cells per sample
average_cells_per_sample <- data_combined %>%
  summarise(average_cells = mean(cell, na.rm = TRUE))

# Print the average number of cells per sample
print("Average number of cells per sample:")
print(average_cells_per_sample)

# Calculate the number of samples with at least 3 cells
samples_with_at_least_3_cells <- data_combined %>%
  filter(cell >= 3) %>%
  summarise(count = n())

# Print the number of samples with at least 3 cells
print("Number of samples with at least 3 cells:")
print(samples_with_at_least_3_cells)

sum(data_combined[, 3], na.rm = TRUE)

