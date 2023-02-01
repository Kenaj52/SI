library(naivebayes)
data = read.csv("~/Desktop/gender_classification_v7.csv")
lista<-list()
for(i in 1:100)
{
xtabs(~ data$gender, data = data)

idx=sample(2,nrow(data),replace=T,prob = c(0.7,0.3))

train=data[idx==1,]
test=data[idx==2,]

model=naive_bayes(as.factor(gender) ~ .,data=train)

#plot(model)

p=predict(model,test)
tab=table(p,test$gender)
#tab

s=sum(diag(tab))/sum(tab)
lista<-append(lista,s)
}
tab
sr=mean(unlist(lista))
sr
od=sd(unlist(lista))
od