# Load necessary libraries
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(gridExtra)
library(dendextend)
library(readxl)

#install 
install.packages('readxl')


# Import Excel file into R
data <- read_excel('C:\\Users\\arpit\\Desktop\\SmartWatch Data File (2).xlsx')

# View entire dataset
data

# Inspect dataset structure
str(data)

# Normalize numerical columns (excluding categorical variables like gender, degree, etc.)
data_scaled <- scale(data[, c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style")])

# Determine optimal number of clusters using Elbow method
wss <- (nrow(data_scaled) - 1) * sum(apply(data_scaled, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(data_scaled, centers = i, nstart = 25)$withinss)
}
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Within Sum of Squares",
     col = "blue", main = "Elbow Method for Optimal Clusters")

# Perform K-Means Clustering (assuming k = 4 from analysis)
set.seed(123)
kmeans_result <- kmeans(data_scaled, centers = 4, nstart = 25)

# Add cluster assignments to original data
data$Cluster <- as.factor(kmeans_result$cluster)

# Compute cluster statistics (Mean & Median)
cluster_summary <- data %>%
  group_by(Cluster) %>%
  summarise(across(ConstCom:Style, list(Mean = mean, Median = median)))

print(cluster_summary)

# Compute hierarchical clustering using Ward's method
dist_matrix <- dist(data_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")


# Convert to dendrogram and enhance visualization
dend <- as.dendrogram(hc)
dend <- color_branches(dend, k = 4)  # Assuming 4 clusters from analysis

ggplot(as.ggdend(dend), theme = theme_minimal()) +
  labs(title = "Dendrogram of Smartwatch Market Segments", x = "Observations", y = "Height") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.ticks.x = element_line())


# Elbow method visualization using factoextra
fviz_nbclust(data_scaled, kmeans, method = "wss") +
  ggtitle("Elbow Method for Optimal Clusters") +
  theme_minimal()

# Compute cluster composition: Number & Percentage of customers in each cluster
cluster_composition <- data %>%
  group_by(Cluster) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

print(cluster_composition)

# Visualize cluster composition with bar plot
#ggplot(cluster_composition, aes(x = Cluster, y = Percentage, fill = Cluster)) +
  #geom_bar(stat = "identity") +
  #labs(title = "Cluster Composition", x = "Cluster", y = "Percentage of Customers") +
  #theme_minimal()

# Custom color palette
cluster_colors <- c("#E74C3C", "#3498DB", "#2ECC71", "#F1C40F")

# Enhanced Cluster Visualization
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster, shape = Cluster)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = cluster_colors) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +
  theme_minimal(base_size = 14) +
  labs(title = "CLUSTER PLOT (4 Segments)", 
       x = "Principal Component 1", 
       y = "Principal Component 2") +
  theme(legend.position = "right", 
        panel.grid.major = element_line(color = "grey80"))

print(p)


