library(caret)
library(ggplot2)
library(dplyr)
library(readr)
install.packages("ggcorrplot")
install.packages("corrplot")
library(ggcorrplot)
library(GmAMisc)
library(VIM)
library(randomForest)
install.packages("plotly")
install.packages("heatmaply")
install.packages("ggcorrplot")
library(heatmaply)
set.seed(123)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
#some exploratory data analysis and visulizations
str(train)
summary(train)
summary(train$SalePrice)
options(scipen=10)

ggplot(train,aes(x=SalePrice,fill=stat(count)))+
  geom_histogram()

ggplot(train,aes(x=log(SalePrice),fill=stat(count)))+
  geom_histogram()


table(train$MSZoning)

ggplot(train, aes(x=MSZoning, y=SalePrice, fill=MSZoning)) + 
  geom_boxplot(alpha=0.3)

table(train$BldgType)

ggplot(train, aes(x=SalePrice , fill = BldgType))+
  geom_histogram()

ggplot(train, aes(SalePrice)) +
  geom_histogram(aes(fill = BldgType), position = position_stack(reverse = TRUE), binwidth = 20000)+
  coord_flip()

ggplot(train, aes(x=SalePrice , fill = as.factor(OverallQual)))+
  geom_histogram()

require(corrplot)
#missing values imputation
test$SalePrice<-rep(NA,1459)
combdata <- rbind(train,test)
colSums(sapply(combdata, is.na))
table(is.na(train$SalePrice))
table(train$TotRmsAbvGrd)

intgers <- c("MSSubClass","LotFrontage","LotArea","MasVnrArea","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","GrLivArea","GarageArea","WoodDeckSF","OpenPorchSF","SalePrice") 
combintgers <- combdata[,intgers]

cor(combintgers,use = "complete.obs")
str(combintgers)
colSums(sapply(combintgers, is.na))
summary(train$LotFrontage)
summary(train$MasVnrArea)

OutVals = boxplot(train$MasVnrArea)$out
which(train$MasVnrArea %in% OutVals)

combdata$LotFrontage[which(is.na(combdata$LotFrontage))] <- mean(combdata$LotFrontage,na.rm = TRUE)
combdata$MasVnrArea[which(is.na(combdata$MasVnrArea))] <- median(combdata$MasVnrArea,na.rm = TRUE)
colSums(sapply(combdata, is.na))
table(combdata$MSZoning)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
 combdata$MSZoning[which(is.na(combdata$MSZoning))] <- Mode(combdata$MSZoning)
 table(combdata$Utilities)
 combdata$Utilities[which(is.na(combdata$Utilities))] <- Mode(combdata$Utilities)
 table(combdata$Exterior1st)
 combdata$Exterior1st[which(is.na(combdata$Exterior1st))] <- Mode(combdata$Exterior1st)
 combdata$Exterior2nd[which(is.na(combdata$Exterior2nd))] <- Mode(combdata$Exterior2nd)
 table(combdata$MasVnrType)
 combdata$MasVnrType[which(is.na(combdata$MasVnrType))] <- Mode(combdata$MasVnrType)
 
 table(combdata$Alley)
 table(combdata$BsmtQual)
 table(combdata$BsmtCond)
 combdata$BsmtCond[which(is.na(combdata$BsmtCond))] <- Mode(combdata$BsmtCond)
 table(combdata$BsmtExposure)
 table(combdata$BsmtFinType1)
 summary(combdata$BsmtFinSF1)
 combdata$BsmtFinSF1[which(is.na(combdata$BsmtFinSF1))] <- median(combdata$BsmtFinSF1,na.rm = TRUE) 
 table(combdata$BsmtFinType2)
 combdata$BsmtFinType2[which(is.na(combdata$BsmtFinType2))] <- Mode(combdata$BsmtFinType2)
 summary(combdata$BsmtFinSF2)
 combdata$BsmtFinSF2[which(is.na(combdata$BsmtFinSF2))] <- median(combdata$BsmtFinSF2,na.rm = TRUE)
 summary(combdata$BsmtUnfSF)
 combdata$BsmtUnfSF[which(is.na(combdata$BsmtUnfSF))] <- median(combdata$BsmtUnfSF,na.rm = TRUE)
 summary(combdata$TotalBsmtSF)
 combdata$TotalBsmtSF[which(is.na(combdata$TotalBsmtSF))] <- median(combdata$TotalBsmtSF,na.rm = TRUE) 
 table(combdata$Electrical)
 combdata$Electrical[which(is.na(combdata$Electrical))] <- Mode(combdata$Electrical) 
 table(combdata$BsmtFullBath)
 combdata$BsmtFullBath[which(is.na(combdata$BsmtFullBath))] <- Mode(combdata$BsmtFullBath)
 table(combdata$BsmtHalfBath)
 combdata$BsmtHalfBath[which(is.na(combdata$BsmtHalfBath))] <- Mode(combdata$BsmtHalfBath)
 table(combdata$KitchenQual)
 combdata$KitchenQual[which(is.na(combdata$KitchenQual))] <- Mode(combdata$KitchenQual)
 table(combdata$Functional)
 combdata$Functional[which(is.na(combdata$Functional))] <- Mode(combdata$Functional)
 table(combdata$FireplaceQu)
 table(combdata$GarageType)
 table(combdata$GarageYrBlt)
 table(combdata$GarageFinish)
 table(combdata$GarageCars)
 combdata$GarageCars[which(is.na(combdata$GarageCars))] <- Mode(combdata$GarageCars)
 summary(combdata$GarageArea)
 combdata$GarageArea[which(is.na(combdata$GarageArea))] <- mean(combdata$GarageArea,na.rm = TRUE) 
 table(combdata$GarageQual)
 combdata$GarageQual[which(is.na(combdata$GarageQual))] <- Mode(combdata$GarageQual)
 table(combdata$GarageCond)
 combdata$GarageCond[which(is.na(combdata$GarageCond))] <- Mode(combdata$GarageCond)
 table(combdata$PoolQC)
 table(combdata$Fence)
 table(combdata$MiscFeature)
 table(combdata$SaleType)
 combdata$SaleType[which(is.na(combdata$SaleType))] <- Mode(combdata$SaleType)
 str(combdata)
 
 combdata <-kNN(combdata , variable = c("Alley","BsmtQual","BsmtExposure","BsmtFinType1","FireplaceQu","GarageType","GarageFinish","PoolQC","Fence","MiscFeature"), k=5)
 
 combdata <- subset(combdata , select = Id:SalePrice)
 #check the corellarion to determine which vatiables affect the sales price 
 cor(as.integer(combdata))

 factorss <- c("GarageFinish","GarageQual","GarageCond","PavedDrive","SalePrice","Fireplaces","MSZoning","Street","LotShape","LandContour","Utilities","Neighborhood","HouseStyle","YearBuilt","Exterior1st","Exterior2nd","MasVnrType","ExterQual")
 facdata <- combdata[1:1460,factorss]
 factorss2 <- c("ExterCond","Foundation","BsmtQual","BsmtCond","BsmtFinType1","Heating","Electrical","Functional","FireplaceQu","GarageType","PoolQC","Fence","MiscFeature","SaleType","SaleCondition","SalePrice")
 facdata2 <- combdata[1:1460,factorss2]
  heatmaply_cor(cor(combintgers[1:1460,]))
 
 str(facdata)
 str(facdata2)
 facdata$GarageFinish <- as.numeric(facdata$GarageFinish) 
 facdata$GarageQual <- as.numeric(facdata$GarageQual)
 facdata$GarageCond <- as.numeric(facdata$GarageCond)
 facdata$PavedDrive <- as.numeric(facdata$PavedDrive)
 facdata$MSZoning <- as.numeric(facdata$MSZoning)
 facdata$Street <- as.numeric(facdata$Street)
 facdata$LotShape <- as.numeric(facdata$LotShape)
 facdata$LandContour <- as.numeric(facdata$LandContour)
 facdata$Utilities <- as.numeric(facdata$Utilities)
 facdata$Neighborhood <- as.numeric(facdata$Neighborhood)
 facdata$HouseStyle <- as.numeric(facdata$HouseStyle)
 facdata$Exterior1st <- as.numeric(facdata$Exterior1st)
 facdata$Exterior2nd <- as.numeric(facdata$Exterior2nd)
 facdata$MasVnrType <- as.numeric(facdata$MasVnrType)
 facdata$ExterQual <- as.numeric(facdata$ExterQual)
 facdata2$ExterCond <- as.numeric(facdata2$ExterCond)
 facdata2$Foundation <- as.numeric(facdata2$Foundation)
 facdata2$BsmtQual <- as.numeric(facdata2$BsmtQual)
 facdata2$BsmtCond <- as.numeric(facdata2$BsmtCond)
 facdata2$BsmtFinType1 <- as.numeric(facdata2$BsmtFinType1)
 facdata2$Heating <- as.numeric(facdata2$Heating)
 facdata2$Electrical <- as.numeric(facdata2$Electrical)
 facdata2$Functional <- as.numeric(facdata2$Functional)
 facdata2$FireplaceQu <- as.numeric(facdata2$FireplaceQu)
 facdata2$GarageType <- as.numeric(facdata2$GarageType)
 facdata2$PoolQC <- as.numeric(facdata2$GarageType)
 facdata2$Fence <- as.numeric(facdata2$Fence)
 facdata2$MiscFeature <- as.numeric(facdata2$MiscFeature)
 facdata2$SaleType <- as.numeric(facdata2$SaleType)
 facdata2$SaleCondition <- as.numeric(facdata2$SaleCondition)
 facdata$BldgType <- as.numeric(facdata$BldgType)
 str(combintgers)
 
 heatmaply_cor(cor(facdata))  
 heatmaply_cor(cor(facdata2))
 #putting the important variables in new data frame
 impvar <- c("GrLivArea","GarageArea","TotalBsmtSF","X1stFlrSF","ExterQual","YearBuilt","GarageFinish","BsmtQual","SalePrice") 
 newData <- combdata[,impvar]
 str(newData)
 
 newtrain <- newData[1:1460,]
 newtest <- newData[1461:2919,]
 #build random forest model
 rf_model <- train(SalePrice~., data=newtrain,method="rf",metric="RMSE",
                   maximize=FALSE,trControl=trainControl(method="repeatedcv",number=5),
                   tuneGrid=expand.grid(mtry = c(5)), importance = T, allowParallel = T, prox = T)
 
 rf_model$results
 varImp(rf_model)
 
 rfpreds <- round(predict(rf_model,newdata = newtest), 2)
 write.csv(data.frame(Id=test$Id,SalePrice=rfpreds),"random_forest_preds.csv",row.names = F)
 #build linear regression model
 lm_model <- train(SalePrice~., data=newtrain, method="lm",metric="RMSE",
                   maximize=FALSE,trControl=trainControl(method = "repeatedcv",number = 10)
 ) 
 lm_model$results
 varImp(lm_model)
 lmpreds <- round(predict(lm_model,newdata = newtest), 2)
 write.csv(data.frame(Id=test$Id,SalePrice=lmpreds),"linear_model_preds.csv",row.names = F)
 
 