# Install necessary libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")

# Load the libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the dataset
dataset <- read_csv("dataset.csv")

# Calculate the average values of acoustic features for the top 10 genres
top_genres <- dataset %>%
  group_by(track_genre) %>%
  summarise(
    avg_acousticness = mean(acousticness, na.rm = TRUE),
    avg_danceability = mean(danceability, na.rm = TRUE),
    avg_energy = mean(energy, na.rm = TRUE),
    avg_speechiness = mean(speechiness, na.rm = TRUE),
    avg_instrumentalness = mean(instrumentalness, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_acousticness)) %>%
  slice(1:10) # Select top 10 genres

# Transform the data for plotting
top_genres_long <- top_genres %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "feature",
    values_to = "value"
  )

# Create the bar chart
ggplot(top_genres_long, aes(x = reorder(track_genre, -value), y = value, fill = feature)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(value, 2)), position = position_dodge(width = 0.9), vjust = -0.3, size = 3.5) +
  labs(
    title = "Average Acoustic Features Across Top 10 Genres",
    x = "Genre",
    y = "Average Value",
    fill = "Feature"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )
