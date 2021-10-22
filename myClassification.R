#######Task 3-Classification - Student Name: Shobhan Mitra,Student ID:45522156####

#Import Packages with Library Command#
library("ggplot2") # Data visualization
library("plotly") # Interactive data visualizations
library("psych") # For correlation visualizations
library("caret") # Machine learning
library("party") # Decision Tree
library("class")
##Task3.1##

#load the preprocessed data file from Task1 and change Gender from factor to number for knn compatible .
ilpd_df <- readRDS(file="ilpd_processed.Rda")
ilpd_df
ilpd_df <- within(ilpd_df, Gender <- factor(Gender, labels = c(1, 2)))
ilpd_df <- within(ilpd_df, Class <- factor(Class, labels = c(1, 2)))
class<-as.integer(ilpd_df$class)
#Divide the dataset into "Training"(70%) & "Test"(30%)
set.seed(45) #seed is a Pseudo random generator. Generates same sequence every time.
indices <- sample(2, nrow(ilpd_df),replace = TRUE, prob = c(0.7, 0.3))
indices
indices == 1
train_data <- ilpd_df[indices == 1,]
test_data <- ilpd_df[indices == 2,]
dim(train_data)
dim(test_data)

summary(train_data)
summary(test_data)
train_data
##Task3.2##
#Learn a classification tree from the training data with default PM by ctree function
library(party)
MyTree1 <- ctree(Class ~. ,data = train_data)
plot(MyTree1, type = "simple")
#evaluate model performance on test data by confusion matrix(using the learned tree predict the clss lebel of test data & calculate accuracy,precision,recall)
prediction_dt <- predict(MyTree1, test_data)
table(prediction_dt, test_data$Class)

##Task3.3##
#Learn a classification tree from the training data with hyperparameters  by ctree function




install.packages("partykit")
library(partykit)

MyTreeTuned1 <- ctree(Class ~.,control=ctree_control(teststat = c("quadratic", "maximum"), splitstat = c("quadratic", "maximum"), 
                                       mincriterion = 0.9999),data = train_data)

plot(MyTreeTuned1, type = "simple")
#evaluate model performance on test data by confusion matrix
prediction_dt <- predict(MyTreeTuned1, test_data)
table(prediction_dt, test_data$Class)

MyTreeTuned2 <- ctree(Class ~.,control=ctree_control(teststat = c("quadratic", "maximum"), splitstat = c("quadratic", "maximum"), 
                                                    mincriterion = 0.60),data = train_data)

plot(MyTreeTuned2, type = "simple")
#evaluate model performance on test data by confusion matrix
prediction_dt <- predict(MyTreeTuned2, test_data)
table(prediction_dt, test_data$Class)

##Task3.4##
#KNN classification method(K=1,2,3,4,5) to predict labels in test subset and calculate confusion matrix#

##k=1##
ilpd_df <- within(ilpd_df, Gender <- factor(Gender, labels = c(1, 2)))
prediction_knn1  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=1, prob=TRUE)
prediction_knn1

#CONFUSION MATRIX
table(prediction_knn1, test_data$Class)

table(prediction_knn1, test_data$Class) %>% prop.table() %>% round(2)

prediction_knn1  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=1, prob=TRUE)

table(prediction_knn1, test_data$Class)

summary(prediction_knn1)
summary(test_data)



##k=2##
prediction_knn2  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=2, prob=TRUE)
prediction_knn2

#CONFUSION MATRIX
table(prediction_knn2, test_data$Class)

table(prediction_knn2, test_data$Class) %>% prop.table() %>% round(2)

prediction_knn2  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=2, prob=TRUE)

table(prediction_knn2, test_data$Class)

summary(prediction_knn2)
summary(test_data)

##k=3##
prediction_knn3  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=3, prob=TRUE)
prediction_knn3

#CONFUSION MATRIX
table(prediction_knn3, test_data$Class)

table(prediction_knn3, test_data$Class) %>% prop.table() %>% round(2)

prediction_knn3  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=3, prob=TRUE)

table(prediction_knn3, test_data$Class)

summary(prediction_knn3)
summary(test_data)

##k=4##
prediction_knn4  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=4, prob=TRUE)
prediction_knn4

#CONFUSION MATRIX
table(prediction_knn4, test_data$Class)

table(prediction_knn4, test_data$Class) %>% prop.table() %>% round(2)

prediction_knn4  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=4, prob=TRUE)

table(prediction_knn4, test_data$Class)

summary(prediction_knn4)
summary(test_data)

##k=5##
prediction_knn5  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=5, prob=TRUE)
prediction_knn5

#CONFUSION MATRIX
table(prediction_knn5, test_data$Class)

table(prediction_knn5, test_data$Class) %>% prop.table() %>% round(2)

prediction_knn5  <- knn(train_data[,-5], test_data[,-5], train_data$Class, k=5, prob=TRUE)

table(prediction_knn5, test_data$Class)

summary(prediction_knn5)
summary(test_data)
