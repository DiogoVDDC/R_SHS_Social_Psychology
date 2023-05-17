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

print(df$attitudes_avg)
print(df$intentions_avg)

# Replace numeric gender codes with descriptive labels
df$gender <- factor(df$gender, 
                            levels = c(1, 2, 3, 4),
                            labels = c('Male', 'Female', 'Non Binary', 'Other'))

# Filter out the 'Other' category
df <- df %>% filter(gender != 'Other')

# Calculate correlation within each gender group
grouped_df <- df %>%
  group_by(gender) %>%
  summarise(correlation = cor(attitudes_avg, intentions_avg, method = "pearson", use = "pairwise.complete.obs"))

# Print correlation
print(grouped_df)

# Create bar plot
p <- ggplot(grouped_df, aes(x = gender, y = correlation, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Correlation between Average Attitude and Intention") +
  theme_minimal() +
  ggtitle("Gender-wise Correlation between \n Average Attitude and Intention Scores") +
  theme(text = element_text(size = 20),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)))

# Print the plot
print(p)
