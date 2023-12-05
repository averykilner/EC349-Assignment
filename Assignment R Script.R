# Assignment Script

# clearing memory
cat("\014")  
rm(list=ls())

# setting working directory
setwd("C:/Users/avery/Desktop/EC349 Assignment")

# installing packages 
install.packages("glmnet")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tree")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")  
install.packages("randomForest") 
install.packages("psych")  
library(glmnet)
library(ggplot2)
library(tidyverse)
library(tree)
library(rpart)
library(rpart.plot)
library(caret)  
library(randomForest) 
library(psych)

# loading dataset
load("yelp_review_small.Rda")

# overview for data understanding, including seeing types of variables
glimpse(review_data_small)

# checking correlation between numeric variables and target variable stars
stars_useful_corr_test <- cor.test(review_data_small$stars, review_data_small$useful)
print(stars_useful_corr_test)

stars_funny_corr_test <- cor.test(review_data_small$stars, review_data_small$funny)
print(stars_funny_corr_test)

stars_cool_corr_test <- cor.test(review_data_small$stars, review_data_small$cool)
print(stars_cool_corr_test)

# converting stars variable from double class numeric into a categorical factor
review_data_small$stars <- factor(review_data_small$stars, levels = 1:5, labels = c("1", "2", "3", "4", "5"))

# visualising distribution of stars with a pie chart
ggplot(review_data_small, aes(x = "", fill = stars)) +
  geom_bar(stat = "count", width = 1) +
  coord_polar("y") +
  theme(axis.text = element_blank(),  
        axis.title = element_blank(),  
        axis.ticks = element_blank()) +
  labs(title = "Pie Chart of Stars in Reviews",
       fill = "Number of Stars")

# converting date variable from character into date type
datetime_string <- "2010-05-03 04:07:13"
my_datetime <- as.POSIXct(datetime_string, format = "%Y-%m-%d %H:%M:%S")
review_data_small$date <- my_datetime

# checking for missing values (of which there are none)
missing_values <- colSums(is.na(review_data_small))
print(missing_values)

# partitioning data
set.seed(1)
train_index <- createDataPartition(review_data_small$stars, p = 0.99284721069828390279073227395755, list = FALSE)
train_data <- review_data_small[train_index, ]
test_data <- review_data_small[-train_index, ]

# modelling with random forest
set.seed(1)
model_RF<-randomForest(stars~.,data=train_data, ntree=100)
pred_RF_test = predict(model_RF, test_data)
mean(model_RF[["err.rate"]])

# evaluating accuracy of model
accuracy <- mean(pred_RF_test == test_data$stars)
print(accuracy)

# calculating Confusion Matrix to find Precision and Recall
conf_matrix <- table(Actual = test_data$stars, Predicted = pred_RF_test)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")

# calculating variable importance
variable_importance <- importance(model_RF)
print(variable_importance)
varImpPlot(model_RF)