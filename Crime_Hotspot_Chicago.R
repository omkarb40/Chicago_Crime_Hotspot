# Step 1: Install and load required packages
install.packages(c("tidyverse", "lubridate", "ggplot2", "cluster", "factoextra", "dendextend", "leaflet"))
library(tidyverse)
library(lubridate)
library(ggplot2)
library(cluster)
library(factoextra)
library(dendextend)
library(leaflet)

# Step 2: Load your dataset
library(readr)
crime_data <- read_csv("Crimes_2020.csv")

# View structure
head(crime_data)
glimpse(crime_data)

# Load libraries again (if not already done)
library(dplyr)
library(lubridate)

# Step 1: Select and clean relevant columns
crime_clean <- crime_data %>%
  select(Date, `Primary Type`, Latitude, Longitude, `Community Area`) %>%
  drop_na()

# Step 2: Convert 'Date' to datetime format and extract useful time features
crime_clean <- crime_clean %>%
  mutate(
    Date = mdy_hms(Date),
    Hour = hour(Date),
    Month = month(Date),
    Crime_Type_Code = as.numeric(as.factor(`Primary Type`))  # Encode crime types numerically
  )

# Step 3: View cleaned dataset
glimpse(crime_clean)

# Create a data frame with relevant features
kmeans_data <- crime_clean %>%
  select(Latitude, Longitude, Crime_Type_Code)

library(factoextra)

# Sample 5000 points for determining k
set.seed(42)
sample_data <- kmeans_data %>% sample_n(5000)

fviz_nbclust(sample_data, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 2) +
  labs(title = "Elbow Method - Optimal k for K-Means (Sample)")

# Final K-Means on full dataset
set.seed(123)
kmeans_result <- kmeans(kmeans_data, centers = 6, nstart = 25)

# Add cluster labels back to main dataset
crime_clean$KMeans_Cluster <- as.factor(kmeans_result$cluster)

library(leaflet)

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

# Sample 500 points from the dataset
set.seed(42)
hier_data <- kmeans_data %>% sample_n(500)

# Compute Euclidean distance
dist_matrix <- dist(hier_data)

# Hierarchical clustering using Ward's method
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, labels = FALSE, hang = -1, main = "Dendrogram - Hierarchical Clustering")
abline(h = 150, col = "red", lty = 2)  # Optional: horizontal line for visual split

# Cut dendrogram into 6 clusters
hc_clusters <- cutree(hc, k = 6)

# Add clusters to sample data
hier_data$HCluster <- as.factor(hc_clusters)

library(ggplot2)

ggplot(hier_data, aes(x = Longitude, y = Latitude, color = HCluster)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Hierarchical Clustering (500 Sample Points)",
       x = "Longitude", y = "Latitude")

# Compute cophenetic correlation
library(dendextend)
coph <- cophenetic(hc)
cor_val <- cor(dist_matrix, coph)
print(paste("Cophenetic correlation coefficient:", round(cor_val, 3)))

library(ggplot2)

crime_clean %>%
  count(`Primary Type`) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(`Primary Type`, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Crime Types in 2020", x = "Crime Type", y = "Count") +
  theme_minimal()

crime_clean %>%
  count(Hour) %>%
  ggplot(aes(x = Hour, y = n)) +
  geom_line(color = "tomato", size = 1.2) +
  geom_point(color = "black") +
  labs(title = "Crime Trend by Hour of the Day", x = "Hour (0–23)", y = "Number of Crimes") +
  theme_minimal()

# Install if not already
install.packages("leaflet.extras")

library(leaflet)
library(leaflet.extras)

# Create interactive crime density heatmap
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
