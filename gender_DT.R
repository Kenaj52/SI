library("party")
library("rpart")
library("rpart.plot")
library(class)
library(caret)

acc = function(x)
{sum(diag(x))/sum(x)}

accuracies <- c()

data=read.csv("~/Desktop/gender_classification_v7.csv")


for(i in 1:100)
{
idx=sample(1:nrow(data),0.9*nrow(data))

train = data[idx,]
test = data[-idx,]

model = ctree(as.factor(gender) ~ ., data=train)

p=predict(model,test)

p=as.factor(p)
test$gender = as.factor(test$gender)

cm <- confusionMatrix(p, test$gender)
accuracy <- acc(cm$table)
accuracies <- c(accuracies, accuracy)
cat(i, "Srednia arytmetyczna: ", accuracy, "\n")
}

mean_accuracy <- mean(accuracies)
sd_accuracy <- sd(accuracies)

cat("Srednia arytmetyczna: ", mean_accuracy, "\n")
cat("Odchylenie standardowe: ", sd_accuracy, "\n")



confusionMatrix(p, test$gender)
plot(model)
