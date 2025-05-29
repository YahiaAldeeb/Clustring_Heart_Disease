# Load required libraries
library(cluster)
library(factoextra)
library(dbscan)
library(scales)
library(caret)
library(gridExtra)
library(ggplot2)

# Load data
df <- read.csv("C:/Users/farida/Downloads/heart_disease.csv", sep = ",")

#read data
dim(df)
str(df)
names(df)

#checking if we have nulls
sum_null<-sum(is.na(df))
sum_null

#drop the null row
df <- na.omit(df) 

#checking duplicates
dup_sum <- sum(duplicated(df))
dup_sum

#removing duplicates
df <- unique(df)

# Drop the ID column cause it's irrelevant
df$id <- NULL 

# Replace outliers function
replace_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data[[column]][data[[column]] < lower_bound] <- lower_bound
  data[[column]][data[[column]] > upper_bound] <- upper_bound
  return(data)
}

#Visualizations for the data
ggplot(df, aes(x = chol)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") + # Adjust binwidth as needed
  labs(title = "Histogram of Cholesterol (chol)", x = "Cholesterol Level", y = "Frequency") +
  theme_bw()
ggplot(df, aes(x = sex)) +
  geom_histogram(fill = "skyblue", color = "black") + # Let ggplot choose binwidth or use methods below
  labs(title = "Histogram of Sex",
       x = "Sex",
       y = "Frequency") +
  theme_bw()
ggplot(df, aes(x = age)) +
  geom_histogram(fill = "skyblue", color = "black") + # Let ggplot choose binwidth or use methods below
  labs(title = "Histogram of Age",
       x = "Age",
       y = "Frequency") +
  theme_bw()
ggplot(df, aes(x =trestbps)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") + # Adjust binwidth as needed
  labs(title = "Histogram of trestbps", x = "Resting Blood Pressure (mmHg)", y = "Frequency") +
  theme_bw()

ggplot(df, aes(x = thalach)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Maximum Heart Rate Achieved (thalach)",
       x = "Maximum Heart Rate Achieved (bpm)",  # Correct x-axis label
       y = "Frequency") +
  theme_bw()

ggplot(df, aes(x = oldpeak)) +
  geom_histogram(fill = "skyblue", color = "black") + # Let ggplot choose a reasonable binwidth or use the methods below
  labs(title = "Histogram of ST Depression Induced by Exercise (oldpeak)",
       x = "ST Depression Induced by Exercise Relative to Rest",
       y = "Frequency") +
  theme_bw()
ggplot(df, aes(x = factor(exang))) + # Convert exang to a factor
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Exercise-Induced Angina (exang)",
       x = "Exercise-Induced Angina",
       y = "Count") +
  scale_x_discrete(labels = c("No", "Yes")) + # More descriptive labels for x axis
  theme_bw()

ggplot(df, aes(x = factor(fbs))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Fasting Blood Sugar (fbs)",
       x = "Fasting Blood Sugar > 120 mg/dl", # Correct x-axis label
       y = "Count") +
  scale_x_discrete(labels = c("False", "True")) + # Corrected labels
  theme_bw()

ggplot(df, aes(x = factor(restecg))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Resting ECG Results (restecg)",
       x = "Resting ECG Result",
       y = "Count") +
  scale_x_discrete(labels = c("Normal", "ST-T Abnormality", "Left Ventricular Hypertrophy")) +
  theme_bw()

ggplot(df, aes(x = factor(slope))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of the Slope of the Peak Exercise ST Segment (slope)",
       x = "Slope of Peak Exercise ST Segment",
       y = "Count") +
  scale_x_discrete(labels = c("Upsloping", "Flat", "Downsloping")) +
  theme_bw()

ggplot(df, aes(x = factor(cp))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Chart of Chest Pain Type (cp)",
       x = "Chest Pain Type",
       y = "Count") +
  scale_x_discrete(labels = c("Typical Angina", "Atypical Angina", "Non-Anginal Pain", "Asymptomatic")) +
  theme_bw()


# Create a box plot
boxplot(df, main="Boxplot of the data", ylab="Values",col=rainbow(8))

# Replace outliers
columns_to_clean <- c("age", "sex", "cp", "trestbps", "chol", 
                      "restecg", "thalach", "exang", "oldpeak", "slope")
for (col in columns_to_clean) {
  df <- replace_outliers(df, col)
}
# Check the removal of outliers using a box plot
boxplot(df, main="Boxplot of the data", ylab="Values",col=rainbow(8))

#variable for scaled data
df_scaled<-df
df_scaled

# Scale numerical features
numeric_cols <- c("age", "trestbps", "chol", "thalach", "oldpeak")
for (col in numeric_cols) {
  df_scaled[[col]] <- scale(df_scaled[[col]])
}
df_scaled

# Elbow method to determine the optimal number of clusters (K-means,K-medoids)
wss <- numeric(16)
for (i in 1:16) {
  wss[i] <- sum(kmeans(df, i)$withinss)
}
plot(1:16, wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within Group Sum of Squares',
     main = 'Elbow Method for Optimal Clusters')
#FROM the graph we concluded that k should be = 3

#K-medoids

# Apply PCA to df (if not done already)
pca_result <- prcomp(df_scaled)
pca_data <- pca_result$x[, 1:2]  # First two principal components

# Perform K-Medoids clustering on df_scaled, and pca_data
pam_scaled <- pam(df_scaled, k = 3)
pam_pca <- pam(pca_data, k = 3)

# Visualize K-Medoids clustering for each data type

# Visualization for df_scaled
fviz_cluster(pam_scaled, data = df_scaled, ellipse.type = "convex", geom = "point") +
  ggtitle("K-Medoids Clustering(Scaled Data)")

# Visualization for pca_data
fviz_cluster(pam_pca, data = pca_data, ellipse.type = "convex", geom = "point") +
  ggtitle("K-Medoids Clustering(PCA Data)")

# Calculate silhouette scores for each clustering result
sil_scaled <- silhouette(pam_scaled$clustering, dist(df_scaled))
sil_pca <- silhouette(pam_pca$clustering, dist(pca_data))

# Calculate and print the average silhouette width for each clustering
cat("Average Silhouette Width for K-Medoids (Scaled Data):", mean(sil_scaled[, 3]), "\n")
cat("Average Silhouette Width for K-Medoids (PCA Data):", mean(sil_pca[,3 ]), "\n")
    
#K-means 

# Perform K-Means clustering on the scaled data (df_scaled)
kmeans_scaled <- kmeans(df_scaled, centers = 3, nstart = 25)

# Perform K-Means clustering on the PCA-transformed data (pca_data)
kmeans_pca <- kmeans(pca_data, centers = 3, nstart = 25)

# Visualize K-Means clustering on the scaled data (df_scaled)
fviz_cluster(kmeans_scaled, data = df_scaled, geom = "point", stand = FALSE, 
             main = "K-Means Clustering (Scaled Data)")

# Visualize K-Means clustering on the PCA-transformed data (pca_data)
fviz_cluster(kmeans_pca, data = pca_data, geom = "point", stand = FALSE, 
             main = "K-Means Clustering (PCA Data)")

# Calculate the silhouette score for the K-Means clustering on the scaled data (df_scaled)
sil_scaled <- silhouette(kmeans_scaled$cluster, dist(df_scaled))

# Calculate the silhouette score for the K-Means clustering on the PCA data (pca_data)
sil_pca <- silhouette(kmeans_pca$cluster, dist(pca_data))

# Print the average silhouette width for each clustering result
cat("Average Silhouette Width for K-Means (Scaled Data):", mean(sil_scaled[, 3]), "\n")
cat("Average Silhouette Width for K-Means (PCA Data):", mean(sil_pca[, 3]), "\n")

# Perform DBSCAN clustering
#lama aamlana scaled data kanet kolaha noise bas
pca_result <- prcomp(df_scaled)
pca_data <- pca_result$x[, 1:2]  # Use first two principal components
k_dist <- kNNdist(pca_data, k = 4)
plot(sort(k_dist), type = "l", main = "k-Distance Plot", xlab = "Points", ylab = "Distance")
abline(h = 0.62, col = "red")
# Apply DBSCAN
dbscan_result <- dbscan(pca_data, eps = 0.62, minPts =4)

# Add cluster labels to the data
pca_data <- as.data.frame(pca_data)
pca_data$cluster <- as.factor(dbscan_result$cluster)

# Visualize the result with ggplot2
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("black", rainbow(length(unique(pca_data$cluster)) - 1))) + 
  labs(title = "DBSCAN Clustering Visualization", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

#Compute silhouette score
dbscan_clusters <- dbscan_result$cluster  # Cluster assignments from DBSCAN
valid_clusters <- dbscan_clusters[dbscan_clusters != 0]  # Exclude noise points (cluster = 0)
valid_data <- pca_data[dbscan_clusters != 0, ]  # Exclude noise points from data

if (length(unique(valid_clusters)) > 1) {
  dbscan_sil <- silhouette(valid_clusters, dist(valid_data))
  avg_silhouette <- mean(dbscan_sil[, 3])  # Average silhouette width
  cat("Average Silhouette Width for DBSCAN:", avg_silhouette, "\n")
} else {
  cat("Silhouette score cannot be computed with fewer than 2 clusters.\n")
}

#Hierarchical Clustering
#using Scaled data
hc_scaled <- hclust(dist(df_scaled), method = "ward.D2")
plot(hc_scaled, main = "Hierarchical Clustering Dendrogram (Scaled Data)", hang = -1, labels = FALSE)
clusters_scaled <- cutree(hc_scaled, k = 3)
silhouette_score_scaled <- silhouette(clusters_scaled, dist(df_scaled))
avg_silhouette_scaled <- mean(silhouette_score_scaled[, 3])
cat("Average Silhouette Width for Hierarchical Clustering (Scaled Data):", avg_silhouette_scaled, "\n")

#using PCA data
hc_pca_data <- hclust(dist(pca_data), method = "ward.D2")
plot(hc_pca_data, main = "Hierarchical Clustering Dendrogram (PCA Data)", hang = -1, labels = FALSE)
clusters_pca <- cutree(hc_pca_data, k = 3)
silhouette_score_pca <- silhouette(clusters_pca, dist(pca_data))
avg_silhouette_pca <- mean(silhouette_score_pca[, 3])  
cat("Average Silhouette Width for Hierarchical Clustering (PCA Data):", avg_silhouette_pca, "\n")
