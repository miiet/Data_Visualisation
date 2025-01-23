# Install and load necessary libraries
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("reshape2")) install.packages("reshape2", dependencies = TRUE)
if (!require("RColorBrewer")) install.packages("RColorBrewer", dependencies = TRUE)

library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# Load the dataset
data <- read.csv("dataset.csv")

# Ensure the genre column is treated as a factor
data$track_genre <- as.factor(data$track_genre)

# Filter for the top 10 genres based on frequency
top_genres <- data %>%
  count(track_genre) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  pull(track_genre)

filtered_data <- data %>%
  filter(track_genre %in% top_genres)

# Compute the correlation matrix for acoustic features
correlation_results <- filtered_data %>%
  select(acousticness, danceability, energy, speechiness, instrumentalness) %>%
  cor()

# Melt the correlation matrix for visualization
correlation_melt <- melt(correlation_results)

# Create the heatmap with values displayed on each tile
ggplot(correlation_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") + 
  geom_text(aes(label = round(value, 2)), color = "black", size = 3.5) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, 
                       limits = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(title = "Heatmap of Acoustic Feature Correlations (Top 10 Genres)",
       x = "Acoustic Features", y = "Acoustic Features") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))
