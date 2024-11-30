
library(tidyverse)
library(caret)
library(ggplot2)

# Load the dataset
sales_data<-file.choose()
sales_data <- read.csv("C:\\Users\\Lenovo\\Desktop\\SalesDataset1.csv")
View(sales_data)

# Assuming you have a target variable 'sales' and predictor variables

# Split the data into training and testing sets
set.seed(123)
trainIndex <-sample(1:nrow(sales_data),0.7*nrow(sales_data))
trainData <- sales_data[trainIndex,]
testData <- sales_data[-trainIndex,]

# Convert Region to a factor (if it is categorical)
trainData$Region <- as.factor(trainData$Region)

# Build the linear regression model
model_lm <- lm(Total.Price ~ Quantity + Unit.Price + Region, data = trainData)

# Summary of the model
summary(model_lm)
# Scatter plot with regression line
ggplot(trainData, aes(x = Unit.Price, y = Total.Price)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(
    title = "Unit Price vs Total Price",
    x = "Unit Price",
    y = "Total Price"
  ) +
  theme_minimal()  # Clean theme



#naive bayes
library(e1071)
library(ggplot2)
# Convert Region to a factor
trainData$Region <- as.factor(trainData$Region)

# Split data into training and testing sets
set.seed(123)  # For reproducibility
split <- sample(2, nrow(trainData), replace = TRUE, prob = c(0.7, 0.3))
train_set <- trainData[split == 1, ]
test_set <- trainData[split == 2, ]
# Build the Naive Bayes model
model_nb <- naiveBayes(Region ~ Quantity + Unit.Price + Total.Price, data = train_set)
# Make predictions
predictions <- predict(model_nb, test_set)

# Confusion matrix
table(Predicted = predictions, Actual = test_set$Region)

# Accuracy
accuracy <- mean(predictions == test_set$Region)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Add predictions to the test set
test_set$Predicted <- predictions

# Visualize the predictions
ggplot(test_set, aes(x = Unit.Price, y = Total.Price, color = Predicted)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Naive Bayes Predictions of Region",
    x = "Unit Price",
    y = "Total Price",
    color = "Predicted Region"
  ) +
  theme_minimal()

#Decision tree
# Load the libraries
library(rpart)
library(rpart.plot)
# Convert Region to a factor (if it's not already)
trainData$Region <- as.factor(trainData$Region)

# Build the decision tree
model_tree <- rpart(Total.Price ~ Quantity + Unit.Price + Region, data = trainData, method = "anova")

# Plot the decision tree
rpart.plot(
  model_tree, 
  type = 2,         # Display split labels on the branches
  extra = 100,      # Show fitted values and percentages
  box.palette = "Blues", # Use a blue color palette for nodes
  shadow.col = "gray",   # Add shadow for better visibility
  main = "Decision Tree for Total Price Prediction"
)


#K-means Clustering
# Load the library
# Set a seed for reproducibility
set.seed(123)

# Select features for clustering
features <- trainData[, c("Quantity", "Unit.Price", "Total.Price")]

# Apply k-Means clustering (let's choose 3 clusters)
kmeans_model <- kmeans(features, centers = 3)

# Plot the clusters
library(ggplot2)
ggplot(trainData, aes(x = Quantity, y = Total.Price, color = factor(kmeans_model$cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "K-Means Clustering", x = "Quantity", y = "Total Price", color = "Cluster") +
  theme_minimal()



