  #Training a model on the data
  selected_variables <- data[, c("age", "Spend_Meat", "Spend_Wine", "Spend_OrganicFood", "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods","Annual_Income", "Purchases_Online", "Purchases_Catalog", "Purchases_Store", "Visits_OnlineLastMonth", "Promo_Purchases","years_with_company", "Kidhome", "Teenhome", "Last_Interaction")] 
  selected_variables_z <- as.data.frame(lapply(selected_variables, scale))
  summary(selected_variables_z)
  
  set.seed(2345)
  customer_clusters <- kmeans(selected_variables_z, 3)
  customer_clusters
  customer_clusters$size
  customer_clusters$centers
  customer_clusters$cluster


#Elbow plot to determine the near optimal number of clusters
k.max <- 10
#data <- scaled_data
wss <- sapply(1:k.max, 
              function(k){kmeans(selected_variables_z, k, nstart=50, iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# we aim for 3 clusters
customer_clusters <- kmeans(selected_variables_z, 3)
customer_clusters

customer_clusters$tot.withinss
customer_clusters$betweenss
customer_clusters$size
customer_clusters$centers

# Step 4: Visualize clusters
library (cluster)
clusplot(selected_variables_z, customer_clusters$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "K-means Cluster Plot")

#SILHOUETTE INDEX
library(cluster)

# Define the range of clusters
k_values <- 1:10
silhouette_scores <- numeric(length(k_values))

# Loop through different cluster sizes and compute silhouette scores
for (k in k_values) {
  if (k == 1) {
    silhouette_scores[k] <- NA  # Silhouette score is not defined for a single cluster
  } else {
    kmeans_model <- kmeans(selected_variables_z, centers = k, nstart = 25)
    sil <- silhouette(kmeans_model$cluster, dist(selected_variables_z))
    silhouette_scores[k] <- mean(sil[, 3])
  }
}

# Print silhouette scores
print(silhouette_scores)

# Optional: Plot the silhouette scores
plot(k_values, silhouette_scores, type = "b", pch = 19, col = "blue", 
     xlab = "Number of Clusters", ylab = "Average Silhouette Score", 
     main = "Silhouette Analysis for K-Means Clustering")
