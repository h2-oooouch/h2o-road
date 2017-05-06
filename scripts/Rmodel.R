#install.packages('caTools')
#install.packages('randomForest')
#install.packages('ElemStatLearn')

library(caTools)
library(randomForest)
library(ElemStatLearn)

# Data

setwd("/home/pablo/SpaceAppChallengue/scripts")
DataE = read.csv('csv_entrenamiento.csv')
DataE$month = as.factor(sapply(DataE$fecha,function(x)unlist(strsplit(as.character(x),'/'))[2]))
DataE = DataE[,colnames(DataE)[c(1:3,9,4:8)]]
colnames(DataE) = c('long', 'lat', 'date', 'month', 'ndvi', 'rain','soil','dem','flood')
DataE$ndvi[DataE$ndvi>1]=0.7 # Correction

# Cross validation datasets

# folds = data.frame('ntree' = 1:800,'pred' = NA)
# 
# for(i in 1:nrow(folds)){
#   
#   split = sample.split(DataE$flood, SplitRatio = 0.75)
#   training_set = subset(DataE, split == TRUE)
#   test_set = subset(DataE, split == FALSE)
#   
 # Random Forest Classification
rfclass = randomForest(x = DataE[,-c(3,9)],
                          y = DataE$flood,
                          ntree = 200)

newData = DataE[DataE$date == levels(DataE$date)[9],]
newData$date = "15/03/2030"
newData$rain = newData$rain + round(rexp(nrow(newData),0.1),1)
newData$ndvi = rnorm(nrow(newData),0.25,0.15)

# Predicting the Test set results
ypred_rf = as.factor(predict(rfclass, newdata = newData[,-c(3,9)]))
csvData = cbind(newData[,1:2], "flood"=ypred_rf)

write.csv(file="Scenario1.csv",x = csvData, quote=FALSE, row.names = FALSE)



# # Making the Confusion Matrix
# t_rf = table(test_set[, 9], ypred_rf)
# folds$pred[i] = sum(diag(t_rf))/sum(t_rf)*100
# print(i)
# }
# plot(folds$ntree,folds$pred)
