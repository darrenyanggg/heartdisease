heart<-read.csv("heart.csv",header=T,stringsAsFactors=TRUE)
attach(heart)
summary(heart)
library(class)
head(heart)



#Logistic Regression- use glm to fit model, take exponentiated coefficients to determine effect of 1 unit increase in variables on odds of heart disease
glm.fit1=glm(HeartDisease~Age+Sex+ChestPainType+RestingBP+Cholesterol+FastingBS+RestingECG+MaxHR+ExerciseAngina+Oldpeak+ST_Slope,family="binomial",data=heart)
summary(glm.fit1)
exp(coef(glm.fit1))

set.seed(1)
k=5
folds=sample(1:k,nrow(heart),replace=TRUE)

accuracy=rep(0,k)
for(i in 1:k)
{
  glm.fit2=glm(HeartDisease~Age+Sex+ChestPainType+RestingBP+Cholesterol+FastingBS+RestingECG+MaxHR+ExerciseAngina+Oldpeak+ST_Slope,family="binomial",data=heart[folds!=i,])
  heart.test=heart[folds==i,]
  glm.probs2 =predict(glm.fit2,heart.test, type="response")
  glm.pred2=rep("0",nrow(heart[folds==i,]))
  
  glm.pred2[glm.probs2>.5]="1"
  
  test.truevalue=HeartDisease[folds==i]
  table(glm.pred2,test.truevalue)
  accuracy[i]=mean(glm.pred2==test.truevalue)
}

table(glm.pred2,test.truevalue)
mean(accuracy)





# Random Forest
heart$HeartDisease<- as.factor(heart$HeartDisease)
library(tree)
library(randomForest)

set.seed(1)


# We want 80% as training so 80-20 holdout
train=sample(nrow(heart),nrow(heart)*0.8)
# To choose all x variables we can use .
# Making the subset as training 
tree.model=tree(HeartDisease~.,heart, subset= train)

# Create testing data set
heart.test= heart[-train,]
# Create the y in the testing data set
HeartDisease.test= HeartDisease[-train]


rf.heart= randomForest(HeartDisease~.,data=heart,subset=train, mtry=3,importance=TRUE)
rf.heart

# Evaluate performance of random forest by fitting it to the testing data set
yhat.rf= predict(rf.heart, newdata= heart[-train,])
# Create the testing data set
heart.test= heart[-train,"HeartDisease"]
heart.test

# MSE of testing data (Testing Error)
mean((as.numeric(yhat.rf)-as.numeric(heart.test))^2)

# Confusion Matrix
table(yhat.rf, heart.test)






# KNN
library(class)

# x values 
xval = cbind(Age,Sex,ChestPainType,RestingBP,Cholesterol,FastingBS,RestingECG,MaxHR,ExerciseAngina,Oldpeak,ST_Slope)


# Create a matrix to initialize accuracy (Matrix of 0s)
# 0 as values, 10 as rows, 5 as columns
accuracy = matrix(0,10,5)
set.seed(1)
# Assign each observation with 1 fold index 
folds = sample(1:5, nrow(xval), replace=TRUE)


# 10 KNN (Rows)
for (j in 1:10)
{
  # 5 Fold Cross Validation Fold (Column)
  for(i in 1:5)
  {
    # x variables in training set
    train.xval=xval[folds!=i,]
    # x variables in testing set
    test.xval = xval[folds==i,]
    # Y in training set
    train.truevalue = HeartDisease[folds!=i]
    # Y in training set
    test.truevalue = HeartDisease[folds==i]
    
    # KNN using training and testing data set by jNN (10NN here)
    knn.pred = knn(train.xval, test.xval, train.truevalue, k=j)
    # Compare predicted value and true value to get accuracy 
    accuracy[j,i]= mean(knn.pred==test.truevalue)
  }
}

# Shows you the accuracy of every single KNN to Cross Validation Combo
accuracy

cv.accuracy = apply(accuracy,1, mean)
cv.accuracy

#Finding best NN by finding highest and matching
max(cv.accuracy) #5NN

