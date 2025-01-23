# ===============================================
# Spotify Music Data Analysis - INF6027 Assignment
# Research Topic: Are Low-Loudness Tracks Always Perceived as Sad?
# ===============================================
# Install required packages
install.packages("tidyverse")  # Collection of packages including ggplot2, dplyr, tidyr etc.
install.packages("corrplot")   # For correlation plots
install.packages("caret")      # For machine learning/modeling
install.packages("randomForest") # For random forest modeling
install.packages("viridis")    # For color palettes
install.packages("gridExtra")  # For arranging multiple plots
install.packages("reshape2")   # For reshaping data
# ==== 1. Setup and Library Loading ====
library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)
library(viridis)
library(gridExtra)
library(reshape2)

# Create output directories
dirs <- c("plots/data_checks", "plots/main", "models")
sapply(dirs, dir.create, showWarnings = FALSE, recursive = TRUE)

# ==== 2. Initial Data Exploration ====
# Load data
music_data <- read.csv("dataset.csv")

# Step 1: Display first few rows and basic structure
print("First few rows of the dataset:")
head(music_data)
print("\nDataset structure:")
str(music_data)

# Step 2: Basic summary statistics
print("\nSummary statistics:")
summary(music_data)

# Step 3: Check dimensions and column names
print("\nDataset dimensions:")
dim(music_data)
print("\nColumn names:")
colnames(music_data)

# Step 4: Check missing values
missing_values <- colSums(is.na(music_data))
print("\nMissing values in each column:")
print(missing_values)

# Step 5: Check data types and unique values in categorical variables
print("\nUnique genres:")
print(length(unique(music_data$track_genre)))
print(head(sort(table(music_data$track_genre), decreasing = TRUE)))

# Step 6: Basic statistics for key numeric variables
numeric_summary <- music_data %>%
  select(loudness, valence, energy, danceability, tempo) %>%
  gather() %>%
  group_by(key) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    min = min(value),
    max = max(value),
    q1 = quantile(value, 0.25),
    q3 = quantile(value, 0.75)
  )
print("\nNumeric variables summary:")
print(numeric_summary)

# ==== 3. Data Quality Checks and Visualization ====
# 1. Distribution plots for key numeric variables
p1 <- ggplot(gather(select(music_data, loudness, valence, energy, danceability)), 
             aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  facet_wrap(~key, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Key Variables")
ggsave("plots/data_checks/distributions.png", p1, width = 12, height = 8)

# 2. Box plots to check for outliers
p2 <- ggplot(gather(select(music_data, loudness, valence, energy, danceability)), 
             aes(y = value)) +
  geom_boxplot(fill = "skyblue") +
  facet_wrap(~key, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots for Outlier Detection")
ggsave("plots/data_checks/boxplots.png", p2, width = 12, height = 8)

# 3. Correlation heatmap
numeric_cols <- c("loudness", "valence", "energy", "danceability", 
                  "tempo", "acousticness", "speechiness", "instrumentalness")
cor_matrix <- cor(music_data[numeric_cols])
png("plots/data_checks/correlation_heatmap.png", width = 800, height = 800)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         addCoef.col = "black",
         title = "Correlation Heatmap of Audio Features")
dev.off()

# 4. Scatter plot matrix for key variables
pairs_data <- music_data %>%
  select(loudness, valence, energy, danceability)
png("plots/data_checks/scatter_matrix.png", width = 1000, height = 1000)
pairs(pairs_data, main = "Scatter Plot Matrix")
dev.off()

# ==== 4. Main Analysis Visualizations ====
# 1. Loudness-Valence Relationship
p3 <- ggplot(music_data, aes(x = loudness, y = valence)) +
  geom_point(aes(color = energy), alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  scale_color_viridis() +
  theme_minimal() +
  labs(title = "Relationship between Loudness and Valence",
       subtitle = "Color indicates Energy level")
ggsave("plots/main/loudness_valence.png", p3, width = 10, height = 8)

# 2. Genre-wise Analysis
genre_summary <- music_data %>%
  group_by(track_genre) %>%
  summarise(
    mean_loudness = mean(loudness),
    mean_valence = mean(valence),
    count = n()
  ) %>%
  filter(count > 100) %>%
  arrange(desc(count))

p4 <- ggplot(genre_summary, 
             aes(x = mean_loudness, y = mean_valence)) +
  geom_point(aes(size = count), alpha = 0.7) +
  geom_text(aes(label = track_genre), size = 3, vjust = 2) +
  theme_minimal() +
  labs(title = "Genre Analysis: Loudness vs Valence",
       size = "Number of Tracks")
ggsave("plots/main/genre_analysis.png", p4, width = 12, height = 8)

# 3. Energy-Loudness Interaction
p5 <- ggplot(music_data, aes(x = energy, y = valence)) +
  geom_point(aes(color = loudness), alpha = 0.6) +
  geom_smooth(method = "lm") +
  scale_color_viridis() +
  theme_minimal() +
  labs(title = "Energy-Valence Relationship",
       subtitle = "Color indicates Loudness level")
ggsave("plots/main/energy_loudness.png", p5, width = 10, height = 8)

# 4. Tempo-Loudness Relationship
p6 <- ggplot(music_data, aes(x = tempo, y = loudness)) +
  geom_point(aes(color = valence), alpha = 0.6) +
  geom_smooth(method = "lm") +
  scale_color_viridis() +
  theme_minimal() +
  labs(title = "Tempo-Loudness Relationship",
       subtitle = "Color indicates Valence")
ggsave("plots/main/tempo_loudness.png", p6, width = 10, height = 8)

# ==== 5. Modeling ====
# Prepare data for modeling
set.seed(123)
train_index <- createDataPartition(music_data$valence, p = 0.8, list = FALSE)
train_data <- music_data[train_index, ]
test_data <- music_data[-train_index, ]

# Model 1: Linear Regression
lm_model <- lm(valence ~ loudness + energy + danceability + tempo + 
                 acousticness + speechiness + instrumentalness,
               data = train_data)
summary(lm_model)
 

# Model Evaluation
predictions_lm <- predict(lm_model, test_data)
predictions_rf <- predict(rf_model, test_data)

rmse_lm <- sqrt(mean((test_data$valence - predictions_lm)^2))
rmse_rf <- sqrt(mean((test_data$valence - predictions_rf)^2))

# Save model performance
model_performance <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  RMSE = c(rmse_lm, rmse_rf) 
)
write.csv(model_performance, "models/performance_metrics.csv")

# Print final results
cat("\nModel Performance:\n")
print(model_performance)