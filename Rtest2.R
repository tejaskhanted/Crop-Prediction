data <- read.csv("F:/Downloads/crop data/Crop_recommendation.csv" , stringsAsFactors =TRUE)
sapply(data, class)
head(data)




summary(data)
library(caret)

# split input and output
x <- data[,1:7]
y <- data[,8]
# boxplot for each attribute on one image
par(mfrow=c(1,7))
for(i in 1:7) {
 boxplot(x[,i], main=names(data)[i])

}


 #barplot for class breakdown
#plot(y)

# scatterplot matrix
#featurePlot(x=x, y=y, plot= "ellipse")













# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)





#Data Partition
#index <- createDataPartition(data$Crop, p = .75, list = FALSE)
#train <- data[index, ]
#test <- data[-index, ]
 #Data Randomly Partiiton
set.seed(2)
dummy_sep <- rbinom(nrow(data) , 1 ,0.7)
test  <- data[dummy_sep == 0, ]
train <- data[dummy_sep == 1, ]
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


# ta=data, method="lda", metric=metric, trControl=control )
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Crop~., data=data, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Crop~., data=data, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Crop~., data=data, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Crop~., data=data, method="rf", metric=metric, trControl=control)
#summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)


# summarize Best Model
print(fit.rf)

#names(fit.rf)


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.rf, test)
#predictions
test$PredictedCrop <- predictions

#confusionMatrix(predictions, test$Crop)


#plot(fit.rf)

newdata <- data.frame(N = 10 , P=26, K= 43 ,temperature = 20 , humidity = 25,ph = 3.0734 , rainfall = 100.0550 )
newdata1 <-data.frame(N =25	,P = 78,	K=76,	temperature = 17.48042641,humidity =	15.7559405, ph =	7.228963452 , rainfall =	66.96980581	)

# Predict sales values
Predited <- predict(fit.rf, newdata1)
Predited
# Predicting Price in test dataset
newdata$PredictedCrop <- Predited







