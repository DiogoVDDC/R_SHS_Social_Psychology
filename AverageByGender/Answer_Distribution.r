# Load the necessary package
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Specify the path to the CSV file
file_path <- "./Group_EF_data.csv"

# Read the CSV file
data <- read_csv(file_path, show_col_types = FALSE)

# Filter the data
filtered_data <- data %>% filter(Finished == 1)

# Replacing 12 with NA in the entire data frame
filtered_data[filtered_data == 12] <- NA

# Reshape the data to long format
long_data <- filtered_data %>% pivot_longer(
  cols = starts_with(c("attitudes")), # or intentions
  names_to = "variable",
  values_to = "value"
)

# Calculate averages and remove NA values
averages <- long_data %>% 
  group_by(gender, variable) %>% 
  summarise(average = mean(value, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(average)) 

# Create bins for the histogram
averages$bins <- cut(averages$average, breaks = seq(from = min(averages$average, na.rm = TRUE), to = max(averages$average, na.rm = TRUE), by = 0.5), include.lowest = TRUE)

averages <- averages %>% filter(!is.na(bins))

# Create the histogram
plot <- ggplot(averages, aes(x = bins)) +
  geom_histogram(stat="count", fill="skyblue", alpha=0.7, color="black", binwidth = 0.5) +
  labs(x = "Average Score", y = "Count", title = "Distribution of Average Intentions Score") +
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    axis.title = element_text(size = 30, margin = margin(t = 10, b = 10)),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(40, 40, 40, 40) # plot padding
  )

# Print the plot
print(plot)
