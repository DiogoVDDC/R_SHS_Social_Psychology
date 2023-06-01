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
  cols = starts_with(c("attitudes", "intentions")),
  names_to = "variable",
  values_to = "value"
)


print(long_data)

# Calculate averages
averages <- long_data %>% 
  group_by(gender, variable) %>% 
  summarise(average = mean(value, na.rm = TRUE), .groups = "drop")

print (averages)

# Split the data into two dataframes based on the variable
attitudes <- averages %>% filter(str_detect(variable, "attitudes"))
intentions <- averages %>% filter(str_detect(variable, "intentions"))

print(attitudes, n=60)
print(intentions)


# Create the bar plots
plot_attitudes <- ggplot(attitudes, aes(x = as.factor(gender), y = average, fill = as.factor(gender))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"),
                    labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  labs(x = "", y = "Average", title = "Average Attitudes by Gender") +
  theme_minimal() +
  theme(
    text = element_text(size = 30),
    axis.title = element_text(size = 30, margin = margin(t = 10, b = 10)),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(30, 30, 30, 30), # plot padding
    legend.position = "none"
  )

plot_intentions <- ggplot(intentions, aes(x = as.factor(gender), y = average, fill = as.factor(gender))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"),
                    labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  labs(x = "", y = "Average", title = "Average Intentions by Gender") +
  theme_minimal() +
  theme(
    text = element_text(size = 30),
    axis.title = element_text(size = 30, margin = margin(t = 10, b = 10)),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(30, 30, 30, 30), # plot padding
    legend.position = "none"
)

# Print the plot
print(plot_attitudes)

# Print the plot
print(plot_intentions)




# Calculate the overall average ratings for attitudes
attitudes_overall <- attitudes %>%
  group_by(gender) %>%
  summarise(average_overall = mean(average, na.rm = TRUE), .groups = "drop")

# Calculate the overall average ratings for intentions
intentions_overall <- intentions %>%
  group_by(gender) %>%
  summarise(average_overall = mean(average, na.rm = TRUE), .groups = "drop")

plot_attitudes_overall <- ggplot(attitudes_overall, aes(x = as.factor(gender), y = average_overall, fill = as.factor(gender))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"),
                    labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  labs(x = "", y = "Average", title = "Overall Average Attitudes by Gender") +
  theme_minimal() +
  theme(
    text = element_text(size = 30),
    axis.title = element_text(size = 30, margin = margin(t = 10, b = 10)),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(30, 30, 30, 30), # plot padding
    legend.position = "none"
  )

# Print the plot
print(plot_attitudes_overall)

# Create the bar plot for overall average intentions by gender
plot_intentions_overall <- ggplot(intentions_overall, aes(x = as.factor(gender), y = average_overall, fill = as.factor(gender))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"),
                    labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  labs(x = "", y = "Average", title = "Overall Average Intentions by Gender") +
  theme_minimal() +
  theme(
    text = element_text(size = 30),
    axis.title = element_text(size = 30, margin = margin(t = 10, b = 10)),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(30, 30, 30, 30), # plot padding
    legend.position = "none"
  )

# Merge attitudes_overall and intentions_overall dataframes
diff_data <- merge(attitudes_overall, intentions_overall, by = "gender")
diff_data$difference <- diff_data$average_overall.y - diff_data$average_overall.x

# Create the bar plot for the difference between average intentions and attitudes
plot_difference <- ggplot(diff_data, aes(x = as.factor(gender), y = difference, fill = as.factor(gender))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  scale_fill_manual(values = c("1" = "blue", "2" = "red", "3" = "green", "4" = "purple"),
                    labels = c("1" = "Male", "2" = "Female", "3" = "Non-binary", "4" = "Other")) +
  labs(x = "", y = "Difference", title = "Difference between Average Intentions \n and Attitudes by Gender") +
  theme_minimal() +
  theme(
    text = element_text(size = 30),
    axis.title = element_text(size = 30, margin = margin(t = 10, b = 10)),
    axis.text.x = element_text(size = 30),
    axis.text.y = element_text(size = 30),
    plot.title = element_text(size = 30, hjust = 0.5, margin = margin(b = 20)),
    plot.margin = margin(30, 30, 30, 30), # plot padding
    legend.position = "none"
  )

# Print the plot
print(plot_difference)


# Print the overall average ratings for attitudes
# print(attitudes_overall)

# Print the overall average ratings for intentions
# print(intentions_overall)