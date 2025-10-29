# ============================================================
# CIS 530 – Advanced Data Mining
# Project: Crime Hotspot Detection Using Clustering (Chicago)
# ============================================================

# ---------------------------
# Step 1: Install & Load Packages
# ---------------------------

# Install required packages if not already installed
packages <- c("tidyverse", "lubridate", "ggplot2", "cluster", 
              "factoextra", "dendextend", "leaflet", "leaflet.extras")
install.packages(setdiff(packages, rownames(installed.packages())))

# Load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cluster)
library(factoextra)
library(dendextend)
library(leaflet)
library(leaflet.extras)
library(readr)

# ---------------------------
# Step 2: Load & Explore Dataset
# ---------------------------

# Load dataset
# https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2/about_data
crime_data <- read_csv("Crimes_2025.csv")

# View structure
head(crime_data)
glimpse(crime_data)

# ---------------------------
# Step 3: Preprocess Data
# ---------------------------

# Select key columns and remove missing data
crime_clean <- crime_data %>%
  select(Date, `Primary Type`, Latitude, Longitude, `Community Area`) %>%
  drop_na()

# Convert Date and create derived time variables
# Convert 'Date' column and filter last 5 years
crime_clean <- crime_data %>%
  select(Date, `Primary Type`, Latitude, Longitude, `Community Area`) %>%
  drop_na() %>%
  mutate(
    Date = mdy_hms(Date),
    Hour = hour(Date),
    Month = month(Date),
    Year = year(Date),  # Extract year
    Crime_Type_Code = as.numeric(as.factor(`Primary Type`))
  ) %>%
  filter(Date >= as.Date(Sys.Date()) - years(5))  # Only include last 5 years

# ---------------------------
# Step 4: K-Means Clustering
# ---------------------------

# Prepare clustering data
kmeans_data <- crime_clean %>%
  select(Latitude, Longitude, Crime_Type_Code)

# Elbow method on a 5,000-point sample to find optimal k
set.seed(42)
sample_data <- kmeans_data %>% sample_n(5000)

fviz_nbclust(sample_data, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2) +
  labs(title = "Elbow Method - Optimal k for K-Means (Sample)")

# Run K-Means on full dataset with k = 6
set.seed(123)
kmeans_result <- kmeans(kmeans_data, centers = 6, nstart = 25)
crime_clean$KMeans_Cluster <- as.factor(kmeans_result$cluster)

# ---------------------------
# Step 5: K-Means Visualization (Leaflet)
# ---------------------------

leaflet(data = crime_clean) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    color = ~KMeans_Cluster,
    radius = 2,
    stroke = FALSE,
    fillOpacity = 0.5,
    popup = ~paste("Crime Type:", `Primary Type`)
  )

# ---------------------------
# Step 6: Hierarchical Clustering
# ---------------------------

# Sample 500 records for efficiency
set.seed(42)
hier_data <- kmeans_data %>% sample_n(500)

# Compute distance matrix and hierarchical clustering
dist_matrix <- dist(hier_data)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Dendrogram - Hierarchical Clustering")
abline(h = 150, col = "red", lty = 2)

# Cut into 6 clusters and add to data
hc_clusters <- cutree(hc, k = 6)
hier_data$HCluster <- as.factor(hc_clusters)

# Visualize clusters
ggplot(hier_data, aes(x = Longitude, y = Latitude, color = HCluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Hierarchical Clustering (500 Sample Points)",
       x = "Longitude", y = "Latitude")

# Cophenetic correlation evaluation
coph <- cophenetic(hc)
cor_val <- cor(dist_matrix, coph)
print(paste("Cophenetic correlation coefficient:", round(cor_val, 3)))

# ---------------------------
# Step 7: Additional Visualizations
# ---------------------------

# Top 10 Crime Types
crime_clean %>%
  count(`Primary Type`) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(`Primary Type`, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Crime Types in 2020", x = "Crime Type", y = "Count") +
  theme_minimal()

# Crime Trend by Hour
crime_clean %>%
  count(Hour) %>%
  ggplot(aes(x = Hour, y = n)) +
  geom_line(color = "tomato", size = 1.2) +
  geom_point(color = "black") +
  labs(title = "Crime Trend by Hour of the Day", x = "Hour (0–23)", y = "Number of Crimes") +
  theme_minimal()

# Interactive Crime Heatmap
leaflet(data = crime_clean) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~Longitude,
    lat = ~Latitude,
    blur = 20,
    max = 0.05,
    radius = 10
  ) %>%
  addLegend(
    position = "bottomright",
    colors = "red",
    labels = "Higher Density",
    title = "Crime Heatmap (2020)"
  )

