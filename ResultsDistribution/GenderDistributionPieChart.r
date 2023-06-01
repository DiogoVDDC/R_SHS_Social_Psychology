# Load the necessary packages
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Specify the path to the CSV file
file_path <- "./Group_EF_data.csv"

# Read the CSV file
data <- read_csv(file_path, show_col_types = FALSE)

# Filter the data
filtered_data <- data %>% filter(Finished == 1)

# Calculate the proportion
filtered_data <- filtered_data %>%
  count(gender) %>%
  mutate(proportion = n / sum(n))

# Add a percentage label
filtered_data$label <- scales::percent(filtered_data$proportion)

# Add labels to genders
filtered_data$gender_labels <- factor(filtered_data$gender, 
                                      levels = c(1, 2, 3, 4),
                                      labels = c('Male', 'Female', 'Non Binary', 'Other'))

# Add label with percentage for the legend, wrap to two lines
filtered_data$legend_labels <- str_wrap(paste(filtered_data$gender_labels, ": ", filtered_data$label), width = 10)

# Create the pie chart
plot <- ggplot(filtered_data, aes(x = "", y = proportion, fill = legend_labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = setNames(c("blue", "red", "green", "purple"), unique(filtered_data$legend_labels))) +
  labs(x = "", y = "", title = "Distribution of Gender for Finished Surveys") +
  theme_minimal() +
  theme(
    text = element_text(size = 30), 
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(40, 40, 40, 40), # plot padding
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Print the plot
print(plot)
