############################ SVM Handwritten Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 Polynomial Kernel
#  4.3 RBF Kernel
# 5. Hyperparameter tuning and cross validation
# 6. Conclusion

#####################################################################################
#1. Business Understanding: 

#The objective is to identify each handwritten digit (0-9) 
#based on the pixel values of each digit

#######################################################################################
#2. Data Understanding:


# Training dataset (mnist_train.csv)
# No.of Observations- 60000
# No.of Attributes- 785

# Test dataset (mnist_test.csv)
# No.of Observations- 10000
# No.of Attributes- 785 


#######################################################################################
#3. Data Preparation:

#Loading Neccessary libraries
library(kernlab)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(gridExtra)
library(e1071)

#Loading Data
train <- read_csv("mnist_train.csv",col_names = FALSE)
test <- read_csv("mnist_test.csv", col_names= FALSE)

#Understanding Dimensions
dim(train)
dim(test)

#Structure of the dataset
str(train)
str(test)

#Printing first few rows
head(train)
head(test)

#Exploring the data
summary(train)
summary(test)

#Checking missing value
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))
sum(is.na(train)) #No missing Values
sum(is.na(test)) #No missing Values

# Consider 15% of train data to make computation faster
set.seed(100)
train.indices = sample(1:nrow(train), 0.15*nrow(train))
train = train[train.indices, ]


#First column represents the digit, so rename it and convert it as factor
colnames(train)[1] <- 'digit'
colnames(test)[1] <- 'digit'
train$digit <- factor(train$digit)
test$digit <- factor(test$digit)

#######################################################################################
#4. Model Building:

##################1. Linear Kernel ################# 
#Building Model
Model_linear <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot")

#Predicting model results 
Eval_linear <- predict(Model_linear, test)

#confusion matrix
confusionMatrix(Eval_linear,test$digit)

####Accuracy : 0.9182 

##################2. Polynomial Kernel ################# 
#Building Model
Model_poly <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "polydot", kpar=list(degree=3))

#Predicting model results 
Eval_poly <- predict(Model_poly, test)

#confusion matrix
confusionMatrix(Eval_poly,test$digit)

####Accuracy : 0.9519

##################3. RBF Kernel ####################### 
#Building Model
Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot")

#Predicting model results 
Eval_RBF <- predict(Model_RBF, test)

#confusion matrix
confusionMatrix(Eval_RBF,test$digit)

####Accuracy : 0.9555

###The accuracy of RBF Kernel is better than those of Polynomial and Linear Kernels.

print(Model_RBF)

#Support Vector Machine object of class "ksvm" 
#
#SV type: C-svc  (classification) 
#parameter : cost C = 1 
#
#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  1.64178908877282e-07 
#
#Number of Support Vectors : 3543 

#######################################################################################
#5. Hyperparameter tuning and Cross Validation

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(100)
grid <- expand.grid(.sigma=c(0.64e-7,1.64e-7,2.64e-7), .C=c(0.1,0.5,1,2))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
#trcontrol = Our traincontrol method

# Performing 5-fold cross validation
fit.svm <- train(digit~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)
# Support Vector Machines with Radial Basis Function Kernel 
# 
# 9000 samples
# 784 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 7200, 7201, 7201, 7199, 7199 
# Resampling results across tuning parameters:
#   
#   sigma     C    Accuracy   Kappa    
# 6.40e-08  0.1  0.8946666  0.8829371
# 6.40e-08  0.5  0.9272234  0.9191230
# 6.40e-08  1.0  0.9366672  0.9296178
# 6.40e-08  2.0  0.9440000  0.9377661
# 1.64e-07  0.1  0.9181118  0.9089973
# 1.64e-07  0.5  0.9479999  0.9422123
# 1.64e-07  1.0  0.9553331  0.9503616
# 1.64e-07  2.0  0.9619996  0.9577701
# 2.64e-07  0.1  0.9253337  0.9170246
# 2.64e-07  0.5  0.9556662  0.9507324
# 2.64e-07  1.0  0.9629998  0.9588820
# 2.64e-07  2.0  0.9672213  0.9635733
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 2.64e-07 and C = 2.

#Plotting the final model
plot(fit.svm)

#We get the highest accuracy of the RBF model with sigma=2.64e-07 and C=2
#We will use this as the final model
final_model <- ksvm(digit~., data=train, scale=FALSE, kernel="rbfdot", kpar=list(sigma=2.64e-07), C=2)
print(final_model)
# SV type: C-svc  (classification) 
# parameter : cost C = 2 
# 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  2.64e-07 
# 
# Number of Support Vectors : 3742 

#Check Training data accuracy of final model
Eval_final_train <- predict(final_model,train)
confusionMatrix(Eval_final_train,train$digit)
#Model accuracy for Training Data is 0.998

#Check Training data accuracy of final model
Eval_final_test <- predict(final_model,test)
confusionMatrix(Eval_final_test,test$digit)
#Model accuracy for Test Data is 0.9657

#######################################################################################
#6. Conclusion

#The final model gives a accuracy of 0.9657 which proves that the SVM model is able 
#to predict the handwritten digits correctly to a large extent.

########################################## END ########################################