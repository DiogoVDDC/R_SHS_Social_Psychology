# Load the necessary packages
library(readr)
library(dplyr)
library(ggplot2)

# Specify the path to the CSV file
file_path <- "./Group_EF_data.csv"

# Read the CSV file
data <- read_csv(file_path, show_col_types = FALSE)

# Filter the data
filtered_data <- data %>% filter(Finished == 1)

# Create the histogram
plot <- ggplot(filtered_data, aes(x=age)) +
  geom_histogram(aes(y = ..density..), binwidth=1, fill="blue", color="black") +
  geom_density(colour = "red", size = 2, alpha = .2) +  # Add density plot with thicker red line
  labs(x="Age", y="Density", title="Distribution of Age") +
  theme_minimal() +
  theme(
    text = element_text(size = 30), 
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(40, 40, 40, 40) # plot padding
  )

# Print the plot
print(plot)
