data.pca <- data
str(data.pca)

#Variable selection and standardization
selected_variables <- data.pca[, c("age", "Spend_Meat", "Spend_Wine", "Spend_OrganicFood", "Spend_WellnessProducts", "Spend_Treats", "Spend_LuxuryGoods","Annual_Income", "Purchases_Online", "Purchases_Store", "Purchases_Catalog", "Visits_OnlineLastMonth", "Promo_Purchases","years_with_company")] 
data.pca_sc <- data.frame(scale(selected_variables))
summary(data.pca_sc)

library(corrplot)
corrplot(cor(data.pca_sc), method = 'ellipse', order="hclust") 

#Perform PCA
data_pca_analysis <- prcomp(data.pca_sc, scale=TRUE) 
summary(data_pca_analysis)

#Visualize PCA Results using Scree Plot
screeplot(data_pca_analysis, type = "lines", main = "Scree Plot")
plot(data_pca_analysis, type="l")

#Cumulative Variance Explained
cumsum(summary(data_pca_analysis)$importance[2,]) 
summary(data_pca_analysis)$importance[3,]

#Create a New Dataset with Selected Components (First 3 Components)
pca_scores <- data_pca_analysis$x[, 1:3]
head(pca_scores)

#Principal Component Loadings
data_pca_analysis$rotation

# Install the necessary packages
install.packages("psych")  # For Varimax rotation
library(psych)

# Apply Varimax rotation using the 'principal' function from the 'psych' package
pca_rotated <- principal(data.pca_sc, nfactors = 3, rotate = "varimax")
pca_rotated$loadings
