# Load the libraries
library(tidyverse)
library(caret)
library(dslabs)

# Load the dataset
data(heights)

# Define the outcome and predictors
y <- heights$sex
x <- heights$height

# Generate training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# Make random predictions
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# Calculate accuracy of random predictions
mean(y_hat == test_set$sex)

# Compare heights in males and females
heights %>% 
  group_by(sex) %>% 
  summarize(mean_height = mean(height), sd_height = sd(height))

# Predict "Male" if height is above 62
y_hat <- ifelse(x > 62, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
mean(y == y_hat)

# Examine accuracy with different cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(cut) {
  y_hat <- ifelse(train_set$height > cut, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

# Visualize accuracy
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line()

# Find the best cutoff
max_accuracy <- max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# Make final predictions using the best cutoff
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
