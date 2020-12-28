#Clearing Environment
rm(list=ls())

#Setting Working directory
setwd("C:\\IIT\\ITMD527 Data Analytics\\KK") 

dataset = read.csv("flood_data.csv")
dataset$AvgTemp = (dataset$MIN_TEMP + dataset$MAX_TEMP)/2
dataset <- dataset[,c(6,10,11)]
dataset = dataset[,c(1,3,2)]

dataset$FLOOD.OCCURRENCE = factor(dataset$FLOOD.OCCURRENCE, levels = c('NO', 'YES'), labels = c(0,1))

#Splitting dataset
library(caTools)
set.seed(100)
split = sample.split(dataset$FLOOD.OCCURRENCE, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature scaling
training_set[,-3] = scale(training_set[,-3])
test_set[,-3] = scale(test_set[,-3])

#Classifier
library(rpart)
classifier = rpart(data = training_set, formula = FLOOD.OCCURRENCE ~ .)

#Predicting test set reuslts, Gives us the probability of 1/0
y_pred = predict(classifier, newdata = (test_set[,-3]), type = 'class')

#Creating Confusion matrix
cm = table(test_set[,3],y_pred)

plot(classifier)
text(classifier)

#Visualizing Training set result 
set = training_set
X1 = seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 = seq(min(set[,2]) - 1 ,max(set[,2]) + 1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('RAINFALL','AvgTemp')
y_grid = predict(classifier, newdata = grid_set[,-3], type = 'class')
plot(set[,-3],
     main = 'Decision Tree (Training set)',
     xlab = 'Rainfall', ylab = 'Temperature',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.',col =ifelse(y_grid ==1, 'springgreen3','tomato'))
points(set, pch = 21,bg =ifelse(set[,3] ==1, 'green4','red3'))

#Visualizing Test set result 
set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 = seq(min(set[,2]) - 1 ,max(set[,2]) + 1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('RAINFALL','AvgTemp')
y_grid = predict(classifier, newdata = grid_set[,-3], type = 'class')
plot(set[,-3],
     main = 'Decision Tree (Test set)',
     xlab = 'Rainfall', ylab = 'Temperature',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.',col =ifelse(y_grid ==1, 'springgreen3','tomato'))
points(set, pch = 21,bg =ifelse(set[,3] ==1, 'green4','red3'))
