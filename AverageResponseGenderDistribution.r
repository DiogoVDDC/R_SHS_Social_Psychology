# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# Specify the path to your CSV file
file_path <- 'Group_EF_data.csv'

# Load the data
df <- read_csv(file_path, show_col_types = FALSE)

# Filter the data
df <- df %>% filter(Finished == 1)

# Replacing 12 with NA in the entire data frame
df[df == 12] <- NA

# Select attitude and intentions columns
attitudes <- df %>%
  select(starts_with('attitudes'))

intentions <- df %>%
  select(starts_with('intentions'))

# Calculate average score for attitudes and intentions
df$attitudes_avg <- rowMeans(attitudes, na.rm = TRUE)
df$intentions_avg <- rowMeans(intentions, na.rm = TRUE)

# Replace numeric gender codes with descriptive labels
df$gender <- factor(df$gender, 
                            levels = c(1, 2, 3, 4),
                            labels = c('Male', 'Female', 'Non Binary', 'Other'))

# Calculate correlation within each gender group
grouped_df <- df %>%
  group_by(gender) %>%
  summarise(correlation = cor(attitudes_avg, intentions_avg, method = "pearson", use = "pairwise.complete.obs"))

# Print correlation
print(grouped_df)

# Create bar plot
p <- ggplot(grouped_df, aes(x = gender, y = correlation, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Correlation between Average Attitude and Intention") +
  theme_minimal() +
  ggtitle("Gender-wise Correlation between Average Attitude and Intention Scores") +
  theme(text = element_text(size = 30),
        plot.margin = margin(1, 1, 1, 1, "cm"))

# Print the plot
print(p)
