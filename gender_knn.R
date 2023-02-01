library(class)
library(caret)
gender_cl=read.csv("~/Desktop/gender_classification_v7.csv")

acc = function(x)
{sum(diag(x))/sum(x)}

nor <- function(x) { (x - min(x)) / (max(x) - min(x)) }

#set.seed(2022)

result <- vector()
listk <- c(2,3,4,5,6,7,8,9)
odchyleniaOdK <- vector()

for(k in 2:9){
  for(i in 1:100){
    idx = sample(1:nrow(gender_cl), 0.9*nrow(gender_cl))
    
    cl = gender_cl$gender
    
    clTrain = cl[idx]
    clTest = cl[-idx]
    
    data_in = gender_cl[,c(1,2,3,4,5,6,7)]
    
    data <- as.data.frame(lapply(data_in, nor))
    
    train = data[idx,]
    test = data[-idx,]
    
    model = knn(train, test, cl=clTrain, k=k)
    
    tab = table(model, clTest)
    
    
    quality = acc(tab)
    
    result <- c(result, quality)
    
    
  }
  cat(k, " Srednia arytmetyczna:", mean(result))
  cat(" Odchylenie Standardowe:", sd(result), "\n")
  
  odchyleniaOdK <- c(odchyleniaOdK, sd(result)) # e 
}

print(tab)

plot(listk, odchyleniaOdK, col='blue')
