library(arules)
library(arulesViz)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(janitor)
library(lubridate)
library(methods)
library(modelr)
library(dplyr)
library(data.table)
library(ggplot2)
library(e1071)
library(corrplot)
library(cluster)
library(cluster)
library(factoextra)

mydata<-read.csv("/Users/karanvir/Desktop/Marketing Analytics/Group A Assignment 3 - Individual submission/OnlineSales.csv")
summary(mydata)
mydata<-clean_names(mydata)
summary(mydata)
any(is.na(mydata))
present <- as.Date("2021-01-30")


mydata <- na.omit(mydata)
mydata$transaction_date <- as.Date(mydata$transaction_date, format = "%m/%d/%y")
rfm <- mydata %>%
  group_by(customer_id) %>%
  summarise(
    Recency = as.numeric(difftime(present, max(transaction_date), units = "days")), # Recency
    Frequency = n(), # Frequency
    MonetaryValue = sum(revenue) # Monetary Value
  )

# Rename columns
colnames(rfm) <- c("CustomerID", "Recency", "Frequency", "MonetaryValue")

rfm$MonetaryValue <- as.integer(rfm$MonetaryValue)
summary(rfm)
# Compute correlation matrix
correlation_matrix <- cor(rfm)

# Create heatmap with annotations
corrplot(correlation_matrix, method = "color", type = "upper", addCoef.col = "black", tl.col = "black", tl.srt = 45)
correlation_matrix <- cor(rfm)

write.csv(rfm, "rfm.csv", row.names = FALSE)

check_skew <- function(df, column) {
  df <- filter_if(df, is.numeric, all_vars((.) != 0))
  # Check if the column exists in the data frame
  if (!(column %in% names(df))) {
    cat("Column", column, "does not exist in the data frame.\n")
    return(NULL)
  }
  
  # Check if the column contains numeric values
  if (!is.numeric(df[[column]])) {
    cat("Column", column, "does not contain numeric values.\n")
    return(NULL)
  }
  # Remove NA values from the specified column
  df <- df[complete.cases(df[[column]]), ]
  
  # Check if the column contains any unique values (constant column)
  if (length(unique(df[[column]])) == 1) {
    cat("Column", column, "contains constant values.\n")
    return(NULL)
  }
  
  # Calculate skewness
  skew <- moments::skewness(df[[column]])
  
  # Check if skewness is NaN
  if (is.nan(skew)) {
    cat("Skewness for column", column, "is NaN. This may be due to highly skewed or constant data.\n")
    return(NULL)
  }
  # Calculate skewness
  skew <- skewness(df[[column]])
  ggplot(df, aes_string(x = column)) +
    geom_density(fill = "skyblue", color = "blue") +
    labs(title = paste("Distribution of", column),
         x = column) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(x = Inf, y = Inf, label = paste("Skew:", round(skew, 2))),
              hjust = 1, vjust = 1, size = 4, color = "red")
  skew_value <<- skew
  cat(paste(column, "'s Skew: ", skew, "\n", sep = ""))
}

#Convert list to data frame
rfm_frame<-as.data.frame(rfm)

# Usage example:
check_skew(rfm, "CustomerID")
# Create a new plot with a 3x1 layout
par(mfrow=c(3,1))
# Plot the skewness for 'Recency'
check_skew(rfm, 'Recency')
# plot the graph
ggplot(rfm_frame, aes(x = Recency, y = skew_value)) + geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Skewness of Receny", x = "Variable", y = "Skewness") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the skewness for 'Frequency'
check_skew(rfm, 'Frequency')
# plot the graph
ggplot(rfm_frame, aes(x = Frequency, y = skew_value)) + geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Skewness of Frequency", x = "Variable", y = "Skewness") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the skewness for 'MonetaryValue'
check_skew(rfm, 'MonetaryValue')
# plot the graph
ggplot(rfm_frame, aes(x = MonetaryValue, y = skew_value)) + geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Skewness of MonetaryValue", x = "Variable", y = "Skewness") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Restore default layout
par(mfrow=c(1,1))


  
rfm_log<-data.table::copy(rfm)
# Drop all data with zero value before taking log
rfm_log <- filter_if(rfm_log, is.numeric, all_vars((.) != 0))
df_rfm_log<-log(rfm_log)

#Convert list to data frame
df_rfm_frame<-as.data.frame(df_rfm_log)

par(mfrow=c(3,1))

# Plot the skewness for 'Recency'
check_skew(df_rfm_log, 'Recency')
# plot the graph
ggplot(df_rfm_frame, aes(x = Recency, y = skew_value)) + geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Skewness of Receny", x = "Variable", y = "Skewness") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the skewness for 'Frequency'
check_skew(df_rfm_log, 'Frequency')
# plot the graph
ggplot(df_rfm_frame, aes(x = Frequency, y = skew_value)) + geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Skewness of Frequency", x = "Variable", y = "Skewness") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot the skewness for 'MonetaryValue'
check_skew(df_rfm_log, 'MonetaryValue')
# plot the graph
ggplot(df_rfm_frame, aes(x = MonetaryValue, y = skew_value)) + geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Skewness of MonetaryValue", x = "Variable", y = "Skewness") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Restore default layout
par(mfrow=c(1,1))
summary(df_rfm_log)

df_rfm_log<-clean_names(df_rfm_log)
summary(df_rfm_log)
# Create a data frame with standardized values
df_rfm_normal<-as.data.frame(scale(df_rfm_log))
df_rfm_normal <- df_rfm_normal[is.finite(rowSums(df_rfm_normal)), ]
summary(df_rfm_normal)
# Set the row names of the new data frame to be the same as the original data frame
rownames(df_rfm_normal)<-rownames(df_rfm_log)

# Set the column names of the new data frame to be the same as the original data frame
colnames(df_rfm_normal)<-colnames(df_rfm_log)
summary(df_rfm_normal)

kmeans_cluster <- function(normalized_df, cluster_number, original_df) {
  # Perform KMeans clustering
  kmeans_model <- kmeans(normalized_df, centers = cluster_number, nstart = 25)
  cluster_labels <- kmeans_model$cluster
  
  # Create a cluster label column in the original dataset
  df_new <- cbind(original_df, cluster = cluster_labels)
  
  return(df_new)
}


fviz_nbclust(df_rfm_normal, kmeans, method = 'silhouette')

silhouette_score <- function(k){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


km.final <- kmeans(df_rfm_normal, 2)
## Total Within cluster sum of square
km.final$tot.withinss
km.final$size
