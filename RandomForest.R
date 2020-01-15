 #Clearing the WOrkspace before loading the data from CSV File 
rm(list = ls(all=TRUE))

#Get Current Working Directory 
getwd()

#Set working directory to the Path where you have files 
setwd('C:\\Harsha\\INSOFE\\Exams\\MITH\\Files')


#load the train and test data from the csv files 
data = read.csv('train_v5-1536323768851.csv',header = TRUE)
tdata = read.csv('test_v5_Student-1536323768851.csv',header = TRUE)


#Load the building structure data set and building owner dataset
bstruct= read.csv('buildingstructure_v6-153632376885.csv' ,header = TRUE)
bowner= read.csv('buildingownership_v5-1536323768851.csv',header = TRUE)

#check the dimensions of the all the data 

dim(data)#249644 13
dim(tdata) #106988 13
dim(bstruct)#356632 17
dim(bowner) # 356632 29



#To check how the data is 
head(bstruct)
head(bowner)
head(data)
head(tdata)

################################# Merging the datasets######################################

#merging the buildingstructure and building owner data set 
merge_1 <- left_join(bowner,bstruct)
dim(merge_1)#356632 42 

#merging the train data 
trainmerge <- left_join(data,merge_1)
dim(trainmerge) #249644 52

#Check the structure of the data 
str(trainmerge)
colnames(trainmerge)

#write the train data in to csv file
train_write = write.csv(trainmerge,file = 'trainmerge.csv',row.names = FALSE)

#merging the testdata
testmerge <- left_join(tdata,merge_1)
dim(testmerge) #106988 52

#Check the structure of test data 
str(testmerge)
colnames(testmerge)

#removing class variable from testdata 
testmerge$damage_grade = NULL
dim(testmerge)  #106988 51
colnames(testmerge)

#write the testdata in to csv file
test_write = write.csv(testmerge,file = 'testmerge.csv',row.names = FALSE)



#Check whether the trainBuildingd and loaded data buildingid are same or not
t1 =data$building_id[!(trainmerge$building_id %in% data$building_id)]
length(t1)


#Check whether the testbuidlingid and loaded  test data buildingid are same or not
t2 =tdata$building_id[!(testmerge$building_id %in% tdata$building_id)]
length(t2)


############################# Merging completed ####################################





####################### Pre Processing ########################
library(caret)
library(DMwR)
library(dplyr)
library(MLmetrics)

#check the structure for train data 
str(trainmerge)

#proportion of target variable
table(trainmerge$damage_grade)
prop.table(table(trainmerge$damage_grade))

#check the structure for test data as well
str(testmerge)

#mutate on train 
train_data <- mutate_all(trainmerge, funs(replace(., .=="", NA)))
sum(is.na(train_data))#47153

#mutate on test 
test_data <- mutate_all(testmerge, funs(replace(., .=="", NA)))
sum(is.na(test_data))#20207

#impute the train data with central imputation 
train_data=centralImputation(train_data)
str(train_data)
sum(is.na(train_data))


#impute the test data with central imputation 
test_data=centralImputation(test_data)
str(test_data)
sum(is.na(test_data))



## Remove columns which does not add any information in train 
x=nearZeroVar(train_data,names=TRUE)
x
length(x)
train_data=train_data[,setdiff(names(train_data),x)]
dim(train_data) #249644 29

## Remove columns which does not add any information in test 
a=nearZeroVar(test_data,names=TRUE)
a
length(a)
test_data=test_data[,setdiff(names(test_data),a)]
dim(test_data) #106988 28


#check whether the columns in train and test are same or not 
z=colnames(train_data) %in% colnames(test_data)
z
colnames(train_data)
colnames(test_data)



#observation All the colnames in train and test are same 


#Check in train 
str(train_data)

#Check proportions of few columns in train_data
table(train_data$has_geotechnical_risk)
table(train_data$has_geotechnical_risk_landslide)
table(train_data$has_repair_started)
table(train_data$has_secondary_use)
table(train_data$has_secondary_use_agriculture)
table(train_data$has_superstructure_adobe_mud) 
table(train_data$has_superstructure_mud_mortar_stone)
table(train_data$has_superstructure_mud_mortar_brick)
table(train_data$has_superstructure_mud_mortar_stone)
table(train_data$has_superstructure_timber)
table(train_data$has_superstructure_bamboo)

#Converting all to categorical 
train_data$has_geotechnical_risk = as.factor(train_data$has_geotechnical_risk)
train_data$has_geotechnical_risk_landslide = as.factor(train_data$has_geotechnical_risk_landslide)
train_data$has_repair_started = as.factor(train_data$has_repair_started)
train_data$has_secondary_use = as.factor(train_data$has_secondary_use)
train_data$has_secondary_use_agriculture = as.factor(train_data$has_secondary_use_agriculture)
train_data$has_superstructure_adobe_mud = as.factor(train_data$has_superstructure_adobe_mud)
train_data$has_superstructure_mud_mortar_stone = as.factor(train_data$has_superstructure_mud_mortar_stone)
train_data$has_superstructure_mud_mortar_brick = as.factor(train_data$has_superstructure_mud_mortar_brick)
train_data$has_superstructure_mud_mortar_stone = as.factor(train_data$has_superstructure_mud_mortar_stone)
train_data$has_superstructure_timber = as.factor(train_data$has_superstructure_timber)
train_data$has_superstructure_bamboo =as.factor(train_data$has_superstructure_bamboo)

str(train_data)

#No need to convert to factors 
#table(train_data$count_floors_pre_eq)
#table(train_data$count_floors_post_eq)
#table(train_data$count_families)



#Check in train 
str(train_data)

#Check proportions of few columns in test_data
table(test_data$has_geotechnical_risk)
table(test_data$has_geotechnical_risk_landslide)
table(test_data$has_repair_started)
table(test_data$has_secondary_use)
table(test_data$has_secondary_use_agriculture)
table(test_data$has_superstructure_adobe_mud) 
table(test_data$has_superstructure_mud_mortar_stone)
table(test_data$has_superstructure_mud_mortar_brick)
table(test_data$has_superstructure_mud_mortar_stone)
table(test_data$has_superstructure_timber)
table(test_data$has_superstructure_bamboo)

#Converting all to categorical 

test_data$has_geotechnical_risk = as.factor(test_data$has_geotechnical_risk)
test_data$has_geotechnical_risk_landslide = as.factor(test_data$has_geotechnical_risk_landslide)
test_data$has_repair_started = as.factor(test_data$has_repair_started)
test_data$has_secondary_use = as.factor(test_data$has_secondary_use)
test_data$has_secondary_use_agriculture = as.factor(test_data$has_secondary_use_agriculture)
test_data$has_superstructure_adobe_mud = as.factor(test_data$has_superstructure_adobe_mud)
test_data$has_superstructure_mud_mortar_stone = as.factor(test_data$has_superstructure_mud_mortar_stone)
test_data$has_superstructure_mud_mortar_brick = as.factor(test_data$has_superstructure_mud_mortar_brick)
test_data$has_superstructure_mud_mortar_stone = as.factor(test_data$has_superstructure_mud_mortar_stone)
test_data$has_superstructure_timber = as.factor(test_data$has_superstructure_timber)
test_data$has_superstructure_bamboo =as.factor(test_data$has_superstructure_bamboo)

str(test_data)

#No need to convert to factors 
#table(test_data$count_floors_pre_eq)
#table(test_data$count_floors_post_eq)
#table(test_data$count_families)


#separate the semicolon in train data 
train_data$conditionofbuildingpostearthquake = gsub(".*;","",train_data$conditionofbuildingpostearthquake)
str(train_data)
train_data$conditionofbuildingpostearthquake = as.factor(train_data$conditionofbuildingpostearthquake)
str(train_data)

#Separate the semicolon in test data 
test_data$conditionofbuildingpostearthquake = gsub(".*;","",test_data$conditionofbuildingpostearthquake)
str(test_data)
test_data$conditionofbuildingpostearthquake = as.factor(test_data$conditionofbuildingpostearthquake)
str(test_data)


# To Reduce the factor variables #not useful because anyway we are removing  ward_id
condenseMe <- function(vector, threshold = 0.001, newName = "Other") {
  toCondense <- names(which(prop.table(table(vector)) < threshold))
  vector[vector %in% toCondense] <- newName
  vector
}

str(train_data)

#keeping a copy of traindata
train_data1 = train_data
str(train_data1)

#keeping a copy of testdata
test_data1 = test_data

#Reducing levels for the ward_id in traindata
train_data$ward_id = as.character(train_data$ward_id)
train_data$ward_id=condenseMe(train_data$ward_id)
table(train_data$ward_id)
train_data$ward_id = as.factor(train_data$ward_id)
str(train_data)

#Reducing levels for the ward_id in testdata
test_data$ward_id = as.character(test_data$ward_id)
test_data$ward_id=condenseMe(test_data$ward_id)
table(test_data$ward_id)
test_data$ward_id = as.factor(test_data$ward_id)
str(test_data)


#converting the damage_grade as factor 
train_data$damage_grade = as.factor(train_data$damage_grade)
str(train_data)

#dividing Data into train and val
set.seed(125)
rows=createDataPartition(train_data$damage_grade,p = 0.7,list = FALSE)
train=train_data[rows,]
str(train)
val=train_data[-rows,]
dim(train)
dim(val)


#removing Unrequired columns from the train  data 
train_data1 = train
train =  train[,setdiff(colnames(train),c('building_id' ,"district_id","ward_id","vdcmun_id"))]
dim(train)

#removing Unrequired columns from the val  data 
Val1 = val
val = val[,setdiff(colnames(val),c('building_id' ,"district_id","ward_id","vdcmun_id"))]
dim(val)



#removing unrequired columns from the test data
test_data1 =test_data
test_data =  test_data[,setdiff(colnames(test_data),c('building_id' ,"district_id","ward_id","vdcmun_id"))]
dim(test_data)




#Buidling random forest on train data 
library(randomForest)
model_rf <- randomForest(damage_grade ~ .,train,ntree = 50,mtry = 5)

#plotting the variables importance
varImpPlot(model_rf)

#Calculating the error metrics on train 
preds_train_rf <- predict(model_rf)
confusionMatrix(preds_train_rf, train$damage_grade)
Accuracy(preds_train_rf, train$damage_grade) #0.8887223  #0.8900213



# Predict on the val data
preds_val_rf <- predict(model_rf, val)
confusionMatrix(preds_val_rf,val$damage_grade)
Accuracy(preds_val_rf,val$damage_grade)  #0.8904823  # 0.8913102

#predict on test data 
preds_test_rf <- predict(model_rf, test_data)
test_output <- data.frame(building_id=test_data1$building_id,damage_grade= preds_test_rf)
#testcsv <-write.csv(test_output, file = "test_1.csv",row.names = FALSE)
testcsv <-write.csv(test_output, file = "test_rf.csv",row.names = FALSE)


#Accuracy is 89.14


##########Buidling Model using c5.0#########################
library(C50)
#Build model on nonsmoted data- train
DT_C50 <- C5.0(damage_grade~.,data=train,rules=TRUE)
summary(DT_C50)

#predict on train
pred_Train = predict(DT_C50,newdata=train, type="class")
confusionMatrix(pred_Train,train$damage_grade)
Accuracy(pred_Train,train$damage_grade) #0.8932

#predict on validation 
pred_val <- predict(DT_C50,newdata = val,type="class")
confusionMatrix(pred_val,val$damage_grade)
Accuracy(pred_val,val$damage_grade)  #0.8862

#predict on test
preds_test <- predict(DT_C50, test_data)
test_output <- data.frame(building_id=test_data1$building_id,damage_grade= preds_test)
testcsv <-write.csv(test_output, file = "test_2.csv",row.names = FALSE) #0.8864


#Model build using Rpart

library(rpart)
library(rpart.plot)

#Build model on train (non smoted data)
DT_rpart_class<-rpart(damage_grade~.,data=train,method="class",control = rpart.control(cp=0.0025740))
DT_rpart_class

#predict on train###
pred_Train = predict(DT_rpart_class,newdata=train, type="class")
confusionMatrix(pred_Train ,train$damage_grade)
Accuracy(pred_Train,train$damage_grade) #0.8770486

#predict on validation###
pred_val = predict(DT_rpart_class, newdata=val, type="class")
confusionMatrix(pred_val,val$damage_grade)
Accuracy(pred_val,val$damage_grade) #0.8765422

#preidct on test ####
preds_test_rf <- predict(DT_rpart_class, test_data,type="class")
test_output <- data.frame(building_id=test_data1$building_id,damage_grade= preds_test_rf)
testcsv <-write.csv(test_output, file = "test_rpar.csv",row.names = FALSE) 

#Accuracy 87.67
 





