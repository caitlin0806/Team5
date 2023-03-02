#Predict Sales using regression trees and related approaches,
# treating the response as a quantitative variable.

#load libraries and inspect data
library(ISLR)
library(tree)
attach(Carseats)
str(Carseats)
summary(Carseats)
?Carseats
set.seed(1)

# (a) Split the data set into a training set and a test set.
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

# (b) Fit a tree to the training set. Plot the tree, and interpret the results. 
# What test MSE do you obtain?
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2) #MSE = 4.922039


# (c) Use cross-validation in order to determine the optimal level of
# tree complexity. Does pruning the tree improve the test MSE?
cv.carseats = cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")


# Best size = 11
#integer requesting the size (i.e. number of terminal nodes) of a specific 
#subtree in the cost-complexity sequence to be returned. 

mse <- rep(NA,18)

for (i in 2:18){
  pruned.carseats = prune.tree(tree.carseats, best = i)
  pred.pruned = predict(pruned.carseats, Carseats.test)
  mse[i] = mean((Carseats.test$Sales - pred.pruned)^2) #MSE = 4.757881
}

best = 1:18
plot(x=best, y=mse)
which.min(mse)
mse[11]

pruned.carseats = prune.tree(tree.carseats, best = 11)
summary(pruned.carseats)

par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

# (d) Use the bagging approach in order to analyze this data. What
# test MSE do you obtain? Use the importance() function to determine which variables are most important.
library(randomForest)

# mtry is # of variables randomly sampled as candidates at each split why 10?
# 10 because length(names(Carseats))-1
bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, 
                            importance = T)
bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2) #MSE = 2.657296

#1st column: mean decrease in accuracy
#2nd column: mean decrease in node impurity
#price, shelveloc, and comprice top 3 important
importance(bag.carseats)
varImpPlot(bag.carseats)

# (e) Use random forests to analyze this data. What test MSE do you
# obtain? Use the importance() function to determine which variables are most important. 
# Describe the effect of m, the number of variables considered at each split, on the error rate obtained.

mse <- rep(NA,length(names(Carseats))-1)

for (i in 2:length(names(Carseats))-1){
  rf.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = i, importance = T)
  rf.pred <- predict(rf.carseats, Carseats.test)
  mse[i] <- mean((Carseats.test$Sales - rf.pred)^2) 
}

mtry = 2:length(names(Carseats))-1
plot(x=mtry, y=mse)
which.min(mse)

#Changing m varies test MSE between 2.599 to 4.795
mse[7] #=2.599039 MSE

#price, shelveloc, and comprice top 3 important
importance(rf.carseats)
varImpPlot(rf.carseats)

# (f) Now analyze the data using BART, and report your results
library(BART)

x <- Carseats[, names(Carseats) != "Sales"]
y <- Carseats[, "Sales"]
xtrain <- x[train,]
ytrain <- y[train]
xtest <- x[-train,]
ytest <- y[-train]

set.seed(1)
bartfit <- gbart(xtrain, ytrain, x.test = xtest)

yhat.bart <- bartfit$yhat.test.mean
mean((ytest - yhat.bart)^2) #1.450842 MSE, best MSE


# regression tree MSE: 4.922039
# pruning: 4.757881
# bagging MSE: 2.657296
# random forest MSE: 2.599039
# BART MSE: 1.450842
