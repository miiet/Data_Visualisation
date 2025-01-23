# Install and load required libraries
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("fmsb")) install.packages("fmsb", dependencies = TRUE)

library(ggplot2)
library(dplyr)
library(fmsb)

# Load dataset
data <- read.csv("dataset.csv")
data$track_genre <- as.factor(data$track_genre)

# Filter for the top 10 genres by frequency
top_genres <- data %>%
  count(track_genre) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  pull(track_genre)

filtered_data <- data %>%
  filter(track_genre %in% top_genres) %>%
  group_by(track_genre) %>%
  summarize(across(c(acousticness, danceability, energy, speechiness, instrumentalness), mean))

# Prepare data for radar chart
radar_data <- as.data.frame(t(filtered_data[-1]))
colnames(radar_data) <- filtered_data$track_genre
radar_data <- rbind(rep(1, ncol(radar_data)), rep(0, ncol(radar_data)), radar_data)

# Define colors
colors <- rainbow(length(top_genres))

# Create radar chart
radarchart(radar_data, axistype = 1, 
           pcol = colors, 
           pfcol = alpha(colors, 0.2), 
           plwd = 2, 
           title = "Genre Profiles: Acoustic Dimensions Comparison Across Top 10 Genres",
           vlcex = 0.9)

# Add legend outside the chart
legend("topright", legend = average_features$track_genre, col = colors, lty = 1, lwd = 4, bty = "n", cex = 0.5,title = "Genres")
