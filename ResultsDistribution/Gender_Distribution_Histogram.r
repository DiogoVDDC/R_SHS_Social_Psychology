# Load the necessary package
library(readr)
library(dplyr)
library(ggplot2)

# Specify the path to the CSV file
file_path <- "../Group_EF_data.csv"

# Read the CSV file
data <- read_csv(file_path, show_col_types = FALSE)

# Filter the data
filtered_data <- data %>% filter(Finished == 1)

# Create the bar plot
plot <- ggplot(filtered_data, aes(x = as.factor(gender), fill = as.factor(gender))) +
  geom_bar(width=0.5) +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"),
                    labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  labs(x = "", y = "Count", title = "Distribution of Gender for Finished Surveys") +
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    axis.title = element_text(size = 30, margin = margin(t = 10, b = 10)),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(40, 40, 40, 40), # plot padding
    legend.position = "none"
  )

# Print the plot
print(plot)