#PART1
setwd("C:/Users/HS/Documents/GitHub/titanic")
train<-read.csv("./data/train.csv", stringsAsFactors=FALSE)
test<-read.csv("./data/test.csv", stringsAsFactors=FALSE)
table(train$Survived)
prop.table(table(train$Survived))
#38% of passengers survived the disaster in the training set.
test$Survived<-rep(0,418)
#it will create one for us and repeat our “0” prediction 418 times

submit<-data.frame(PassengerID=test$PassengerId, Survived=test$Survived)
write.csv(submit,file="theyallperish.csv",row.names=FALSE)

#PART2
summary(train$Sex)
table(train$Sex)
prop.table(table(train$Sex, train$Survived),1)
test$Survived<-0
test$Survived[test$Sex=="female"]<-1

summary(train$Age)
train$Child<-0
train$Child[train$Age<18]<-1
aggregate(Survived~Child+Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

attach(train)
Fare2<-'30+'
Fare2[Fare<30 & Fare>=20]<-'20-30'
Fare2[Fare<20 & Fare>=10]<-'10-20'
Fare2[Fare<10]<-'<10'
detach(train)
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

attach(test)
Survived<-0
Survived[Sex=="female"]<-1
Survived[Sex=="female" & Pclass==3 & test$Fare>=20]<-0
detach(test)

#PART3
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit)
text(fit)
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data=train,
               method="class", 
               control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               data=train,
               method="class",
               control=rpart.control( your controls ))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)
