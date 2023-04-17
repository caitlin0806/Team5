# MSBA Team 5
# Caitlin Bryant, Skyler Liu, Danilo Nikcevic, Cory Wall
# ML TP02 - petfinder

# Clear Variables
rm(list=ls())

# Load data from train dataset
petfinder <- read.csv("C:/Users/17578/ML2/ML2 Final Project/train.csv")

# Attach the dataset
attach(petfinder)

# Remove columns that are not useful -- RescuerID, Description, PetID
petfinder <- petfinder[, -c(19, 21, 22)]

# Check for NA data points --  there is none
sum(is.na(petfinder$AdoptionSpeed))

# Change all data into numeric or factor for further analysis

#1 is dog 0 is cat
petfinder$Type <- as.factor(ifelse(petfinder$Type == 1, "Dog", "Cat"))
#0 is blank name and 1 is named
petfinder$Name <- as.factor(ifelse(petfinder$Name == "", 0, 1))
#age in months
petfinder$Age <- as.numeric(Age)
petfinder$Breed1 <- as.numeric(Breed1)
petfinder$Breed2 <- as.numeric(Breed2)
#1 is male 2 is female 3 is mixed
petfinder$Gender <- as.factor(Gender)
petfinder$Color1 <- as.numeric(Color1)
petfinder$Color2 <- as.numeric(Color2)
petfinder$Color3 <- as.numeric(Color3)
#1 = Small, 2 = Medium, 3 = Large, 4 = Extra Large, 0 = Not Specified
petfinder$MaturitySize <- as.factor(MaturitySize)
#1 = Short, 2 = Medium, 3 = Long, 0 = Not Specified
petfinder$FurLength <- as.factor(FurLength)
#1 = Yes, 2 = No, 3 = Not Sure
petfinder$Vaccinated <- as.factor(Vaccinated)
#1 = Yes, 2 = No, 3 = Not Sure
petfinder$Dewormed <- as.factor(Dewormed)
#1 = Yes, 2 = No, 3 = Not Sure
petfinder$Sterilized <- as.factor(Sterilized)
#1 = Healthy, 2 = Minor Injury, 3 = Serious Injury, 0 = Not Specified
petfinder$Health <- as.factor(Health)
petfinder$Quantity <-as.numeric(Quantity)
petfinder$Fee <-as.numeric(Fee)
#State location in Malaysia
petfinder$State <- as.factor(State)
petfinder$VideoAmt <- as.numeric(VideoAmt)
petfinder$PhotoAmt <-as.numeric(PhotoAmt)
#0: same day, 1: 1st week, 2: 1st month, 3: 2nd/3rd month, 4: >100 days
#totals: 0:410 1:3090 2:4037 3:3259 4:4197
petfinder$AdoptionSpeed <- as.factor(AdoptionSpeed)

#10 factors 10 numeric
str(petfinder)

#Create visualizations
library(ggplot2)
library(tidyverse)

# Type Plot
petfinder %>%
  ggplot(aes(x = Type, fill = Type)) +
  geom_bar(stat = "count", color = "black") +
  theme_minimal() +
  ylab(NULL) +
  scale_fill_brewer(palette="OrRd") +
  theme(legend.position = "none") +
  ggtitle("Count of Cats vs Dogs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

# Gender Plot
petfinder %>%
  ggplot(aes(x = Gender, fill = Gender)) +
  geom_bar(stat = "count", color = "black") +
  theme_minimal() +
  ylab(NULL) +
  scale_fill_brewer(palette="OrRd") +
  scale_x_discrete(labels = c("Male", "Female", "Mixed")) +
  theme(legend.position = "none") +
  ggtitle("Count of Gender") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

# Adoption Speed Plot
petfinder %>%
  ggplot(aes(x = AdoptionSpeed, fill = AdoptionSpeed)) +
  geom_bar(stat = "count", color = "black") +
  theme_minimal() +
  ylab(NULL) +
  scale_fill_brewer(palette="OrRd") +
  scale_x_discrete(labels = c("Same Day", "1st Week", "1st Month", "2nd/3rd Month", ">100 Days")) +
  theme(legend.position = "none") + 
  ggtitle("Count of Different Levels of Adoption Speed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) 

set.seed(1693)
# Split data - Train 70% vs. Test 30%
train <- sample(1:nrow(petfinder), nrow(petfinder) * 0.7)
train.petfinder <- petfinder[train, ] #10,495 observations
test.petfinder <- petfinder[-train, ] #4,498 observations

#########################################LDA##########################################

#Load Libraries
library(MASS)
library(tidyverse)
library(caret)

##center and scale the data
set.seed(1693)
preprocessing <- train.petfinder %>% preProcess(method=c("center","scale"))
traintransformed<-preprocessing %>% predict(train.petfinder)
testtransformed <- preprocessing %>% predict(test.petfinder)

##train model with training set
LDAmodel <- lda(AdoptionSpeed~., data=traintransformed)
LDAmodel

##predictions
set.seed(1693)
lda_predictions<-LDAmodel %>% predict(testtransformed)

#get accuracy rate
lda_accuracy <- mean(lda_predictions$class==testtransformed$AdoptionSpeed) 
lda_accuracy ############################################## ACCURACY = 35.79%

table(lda_predictions$class,testtransformed$AdoptionSpeed)
#observed is y axis predicted is x axis

# 0   1   2   3   4
# 0   0   2   1   0   0
# 1  36 231 191 124 122
# 2  39 438 534 402 377
# 3   7  60 122 156  67
# 4  31 204 361 304 689

##graph
library(ggplot2)
ldaforgraph <- cbind(traintransformed, predict(LDAmodel)$x)

ggplot(ldaforgraph, aes(LD1, LD2)) + geom_point(aes(color=AdoptionSpeed, shape=AdoptionSpeed))
ggplot(ldaforgraph, aes(LD1, LD3)) + geom_point(aes(color=AdoptionSpeed, shape=AdoptionSpeed))
ggplot(ldaforgraph, aes(LD1, LD4)) + geom_point(aes(color=AdoptionSpeed, shape=AdoptionSpeed))
ggplot(ldaforgraph, aes(LD2, LD3)) + geom_point(aes(color=AdoptionSpeed, shape=AdoptionSpeed))
ggplot(ldaforgraph, aes(LD2, LD4)) + geom_point(aes(color=AdoptionSpeed, shape=AdoptionSpeed))
ggplot(ldaforgraph, aes(LD3, LD4)) + geom_point(aes(color=AdoptionSpeed, shape=AdoptionSpeed))


####################################RANDOM FOREST#########################################
#separate x and y variables
X <- petfinder[,-21]
Y <- petfinder[,21]

set.seed(1693)

#make appropriate x train, x test, y train, y test
train <- sample(1:nrow(petfinder), nrow(petfinder) * 0.7)
X_train <- X[train,]
Y_train <- Y[train]
X_test <- X[-train,]
Y_test <- Y[-train]

#Load libraries
library(randomForest)

set.seed(1693)
#create initial random forest model with default mtry
RFmodel <- randomForest(x = X_train, y = Y_train, ntree = 500, importance = TRUE)

#predict the initial random forest model on the xtest dataset
RFpredictions <- predict(RFmodel, X_test)
table(RFpredictions, Y_test) 

# 0   1   2   3   4
# 0   2   7   1   1   0
# 1  38 308 221 140 126
# 2  33 338 481 304 228
# 3  13  90 184 236 102
# 4  27 192 322 305 799

#get accuracy rate
rf_accuracy <- mean(RFpredictions==Y_test) 
rf_accuracy ################################################ ACCURACY=40.60%

#Age, Sterilized, Breed1, PhotoAmt are most important factors
importance(RFmodel)
varImpPlot(RFmodel)

#tune random forest model -- vary mtry between 1:20 to see if accuracy can get better
set.seed(1693) #TAKES A LONG TIME TO RUN
rf_accuracy_tune <- rep(NA,length(names(petfinder))-1)

for (i in 2:length(names(petfinder))-1){
  set.seed(1693)
  rf.petfinder <- randomForest(x = X_train, y = Y_train, ntree = 500, importance = TRUE, mtry=i)
  rf.pred <- predict(rf.petfinder, X_test)
  rf_accuracy_tune[i] <- mean(rf.pred == Y_test)
}

mtry = 2:length(names(petfinder))-1

#plot mtry against accuracy
plot(x=mtry, y=rf_accuracy_tune)

#get highest accuracy
which.max(rf_accuracy_tune)


#Best accuracy via tuning random forest was when mtry is 13
rf_accuracy_tune_max <- rf_accuracy_tune[13] 
rf_accuracy_tune_max #################################### ACCURACY=40.62% 

#Age, Breed1, Sterilized, Photo Amt most important still
importance(rf.petfinder)
varImpPlot(rf.petfinder)

#############################################XGBOOST#############################################
#XGBoost Model

# Load libraries
library(xgboost)

set.seed(1693)
# Put Training X and Y in matrix form:
xtrain <- model.matrix(AdoptionSpeed~., train.petfinder)[ ,-21]
ytrain <- train.petfinder$AdoptionSpeed
ytrain <- as.matrix(ytrain)
set.seed(1693)
# Put Testing X and Y in matrix form:
xtest <- model.matrix(AdoptionSpeed~., test.petfinder)[ ,-21]
ytest <- test.petfinder$AdoptionSpeed
ytest <- as.matrix(ytest)


# Create Initial xgboost Model
set.seed(1693)
xgboost <- xgboost(data=xtrain, label=ytrain, eta=0.5, nround=5, 
                    max.depth=2, verbose=0, objective="multi:softmax", num_class=5)
# eta = learning rate (step size of each boosting step)
# max.depth = maximum depth of tree, 2 splits
# nrounds = max number of iterations 
# objective is multi softmax for multi classification dependent variable
# num_class is 5 since there are 5 classes in the dependent variable

# Make Predictions
set.seed(1693)
xgb_pred <- predict(xgboost, newdata=xtest)
table(xgb_pred, ytest)

# 0   1   2   3   4
# 1  27 125 108  82  62
# 2  49 525 619 427 344
# 3   4  56 111 121  47
# 4  33 229 371 356 802

xgboost_accuracy <- mean(xgb_pred==ytest) 
xgboost_accuracy ###ACCURACY RATE = 37.06%



# Tune eta: Loop through learning rates from 0 to 2, interval=0.1 to see if accuracy increases
set.seed(1693)
xgboost_accuracy_tune <- c()
eta_seq <- seq(0, 2, 0.1) 
index <- 1
for (j in eta_seq){
  set.seed(1693)
  xgboost_tune <- xgboost(data=xtrain, label=ytrain, eta=j, nround=5, max.depth=2, verbose=0, objective="multi:softmax", num_class=5)
  xgb_pred_tune <- predict(xgboost_tune, newdata=xtest)
  xgboost_accuracy_tune[index] <- mean(xgb_pred_tune==ytest)
  index <- index + 1
}



#plot eta against accuracy
plot(x=eta_seq, y=xgboost_accuracy_tune)

#get highest accuracy
which.max(xgboost_accuracy_tune)
xgboost_accuracy_tune


#Changing eta varies accuracy 
xgboost_accuracy_tune[12] 
eta_seq[12]
xgboost_accuracy_tune_max <- xgboost_accuracy_tune[12]
xgboost_accuracy_tune_max ################################### ACCURACY=38.35%  at eta=1.1


########################NEURAL NETWORK#########################################
#convert factor independent variables to numeric
str(petfinder)
petfinder$Type <- as.numeric(as.factor(petfinder$Type))
petfinder$Name<- as.numeric(as.factor(petfinder$Name))
petfinder$Gender <- as.numeric(as.factor(petfinder$Gender))
petfinder$MaturitySize <- as.numeric(as.factor(petfinder$MaturitySize))
petfinder$FurLength <- as.numeric(as.factor(petfinder$FurLength))
petfinder$Vaccinated <- as.numeric(as.factor(petfinder$Vaccinated))
petfinder$Dewormed <- as.numeric(as.factor(petfinder$Dewormed))
petfinder$Sterilized <- as.numeric(as.factor(petfinder$Sterilized))
petfinder$Health <- as.numeric(as.factor(petfinder$Health))
petfinder$State <- as.numeric(as.factor(petfinder$State))


#Load Libraries
library(MASS)
library(tidyverse)
library(caret)

#install.packages("keras")
library(keras)

#devtools::install_github("rstudio/tensorflow")
library(tensorflow)
#install_tensorflow()


set.seed(1693)
# Split data - Train 70% vs. Test 30%
train <- sample(1:nrow(petfinder), nrow(petfinder) * 0.7)
train.petfinder <- petfinder[train, ] #10,495 observations
test.petfinder <- petfinder[-train, ] #4,498 observations

set.seed(1693)
#scale the data
train_data_scaled <- scale(train.petfinder[, 1:20])
test_data_scaled <- scale(test.petfinder[, 1:20])

set.seed(1693)
train_labels <- to_categorical(train.petfinder[, 21])
test_labels <- to_categorical(test.petfinder[, 21])

set.seed(1693)
#created a sequential model with an input layer that takes an input shape of 20
#since that is the number of independent variables, has a relu activation and 64 units
#added a dropout layer with a rate of 20% to combat over fitting
#added another dense layer with 128 units and sigmoid activation 
#added another dropout layer with a rate of 20% to combat over fitting
#added an output layer with 5 units since that is the number of classes
#in the dependent variable, and softmax activation since it is used for multi-classifcation
#compiled the model with a categorical crossentropy loss function 
#that calculates the error between true and predicted values
#used adam optimizer to minimize the error between true and predicted labels
#used accuracy as metric
#fit the model using the scaled train data and train labels using 50 epochs
# 32 for batch size and 0.2 for validation split

NNmodel <- keras_model_sequential()
set.seed(1693)

NNmodel %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(20)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 128, activation = "sigmoid") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 5, activation = "softmax")

set.seed(1693)

NNmodel %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

set.seed(1693)

history <- NNmodel %>% fit(
  train_data_scaled, train_labels,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate model on test set
NN_accuracy <- NNmodel %>% evaluate(test_data_scaled, test_labels, verbose = 0)
NN_accuracy <- NN_accuracy[2] ######################## ACCURACY=38%


#CONCLUSION
lda_accuracy #35.79% ACCURACY
rf_accuracy #40.60% ACCURACY
rf_accuracy_tune_max #40.62% ACCURACY
xgboost_accuracy #37.06% ACCURACY
xgboost_accuracy_tune_max #38.35% ACCURACY
NN_accuracy #38% ACCURACY

#Tuning the Random Forest gave the best accuracy at 40.62%

