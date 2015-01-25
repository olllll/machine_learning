##############
#documentación
#
#http://groupware.les.inf.puc-rio.br/har
#############

##########
#Naive Bayes & Optimality
#http://www.aaai.org/Papers/FLAIRS/2004/Flairs04-097.pdf
###########


#obtener datos
########################
setwd("~/MachLearn//W3//Project")
if(!file.exists("./data")) dir.create("./data")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "./data/pml-training.csv", method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "./data/pml-testing.csv", method = "curl")
#########################

#leer datos
setwd("~/MachLearn/W3/Project")
training<-read.csv("./data/pml-training.csv", header = T, stringsAsFactors = F, na.strings = c("", "#DIV/0!"))

testing<-read.csv("./data/pml-testing.csv", header = T, stringsAsFactors = F)

#crear  train-test
library(caret)
set.seed(52147)
#antes de separar el dataset
training$classe<-as.factor(training$classe)
inValidation<-createDataPartition(y=training$classe, p = 0.4, list = F)
validation<-training[inValidation, ]
training<-training[-inValidation, ]

#retirar na
training<-training[, -(1:7)] #los índices, nombres... menos el num_window
#some variables have been imported as strings, probably due to NA/strange values in the column. Force numeric conversion
training[1:(dim(training)[2]-1)]<-sapply(training[1:(dim(training)[2]-1)], function(x) as.numeric(x))
#remove NA
training<-training[, colSums(is.na(training))== 0]


library(corrgram)
corrgram(training)
class(training$classe)

pairs(training[, 1:5], col = training$classe)
pairs(training[, 6:10], col = training$classe, alpha = 0.3)
pairs(training[, 11:15], col = training$classe, alpha = 0.3)
pairs(training[, 16:20], col = training$classe)
pairs(training[, 21:25], col = training$classe)
pairs(training[, 26:30], col = training$classe)
pairs(training[, 31:35], col = training$classe)
pairs(training[, 36:40], col = training$classe)
pairs(training[,41:45], col = training$classe)
pairs(training[,46:50], col = training$classe)
pairs(training[,51:53], col = training$classe)

names(training)
predictors1<-c("pitch_arm", "yaw_arm", "total_accel_arm", "roll_dumbbell", "pitch_dumbbell", "yaw_dumbbell", "accel_dumbbell_x", "accel_dumbbell_y", "magnet_dumbbell_x", "magnet_dumbbell_z", "roll_forearm", "pitch_forearm")
train1<-subset(training, select =names(training) %in% predictors1)
train1$classe<-training$classe

dim(train1)
names(train1)

head(train1)
#naiveBayes
model<-train(classe ~., method = "nb", trControl = trainControl(method = "cv"), data = train1)

#random Forest
set.seed(255)
randomF<-train(classe ~., method = "rf", trControl = trainControl(method ="cv"), data = train1)
pred<-predict(randomF, newdata = validation)
mean(pred == validation$classe) #0.948
confusionMatrix(pred, validation$classe)
#random forest seems to perform better predicting which class is not than the correct class itself

#bagging

bagLDA<-train(classe ~., method = "bag", B = 10, bagControl = bagControl(
  fit = ldaBag$fit,
  predict = ldaBag$pred, 
  aggregate = ldaBag$aggregate),
  data = train1)
pred2<-predict(bagLDA$aggregate, newdata = validation)
mean(pred2 == validation$classe) #45%

#boosting with trees (gbm)
modelBoost<-train(classe ~., method = "gbm", trControl = trainControl(method = "cv"), data = train1, verbose = F)

pred3<-predict(modelBoost, newdata = validation)
confusionMatrix(pred3, validation$classe)
modFit<-train(wage~., method = "gbm", data = training, verbose =F)

#svm
modelSVM<-train(classe ~., method ="svmLinear", trControl = trainControl(method = "cv"), data = train1)
pred4<-predict(modelSVM, newdata = validation)
confusionMatrix(pred4, validation$classe)

#try another random forest
#random Forest
set.seed(255)
rrandomF<-train(classe ~., method = "rf", 
               trControl = trainControl(
                 method ="repeatedcv", 
                 repeats = 10), 
               data = train1)
pred5<-predict(rrandomF, newdata = validation)
mean(pred5 == validation$classe) #0.948
confusionMatrix(pred5, validation$classe)
#random forest seems to perform better predicting which class is not than the correct class itself


##
trainerror<-subset(training, training$classe != "A")
dim(trainerror)
training %>% group_by(classe) %>% summarise(total = n())


pairs(trainerror[, 1:5], col = trainerror$classe)
pairs(trainerror[, 6:10], col = trainerror$classe)
pairs(trainerror[, 11:15], col = trainerror$classe)
pairs(trainerror[, 16:20], col = trainerror$classe)
pairs(trainerror[, 21:25], col = trainerror$classe)
pairs(trainerror[, 26:30], col = trainerror$classe)
pairs(trainerror[, 31:35], col = trainerror$classe)
pairs(trainerror[, 36:40], col = trainerror$classe)
pairs(trainerror[,41:45], col = trainerror$classe)
pairs(trainerror[,46:50], col = trainerror$classe)
pairs(trainerror[,51:53], col = trainerror$classe)


predictors2<-append(predictors1, c("roll_arm", "magnet_forearm_z"))
##rf2
set.seed(255)
randomFF<-train(classe ~., method = "rf", trControl = trainControl(method ="cv"), data = train1)
pred6<-predict(randomFF, newdata = validation)
mean(pred6 == validation$classe) #0.948
confusionMatrix(pred6, validation$classe)

system.time(train(classe ~., method = "rf", trControl = trainControl(method ="cv"), data = train1))

answers<-predict(randomFF, newdata = testing)

if(!file.exists("./solutions")) dir.create("./solutions")
setwd("./solutions")
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)

barplot(vars)
abline(h = 15000, col = "red")
sum(vars<1)
pairs(training[, sapply(training, var)>1])
names(training[, var(training)>10000])
hm<-as.matrix(training[, -dim(training)[2]])
heatmap(hm)


names(training)
corrgram(training[, 54:57])

ggplot(training, aes(x=magnet_arm_ratioxz, y=magnet_arm_ratioxy, colour = magnet_arm_ratioyz)) + 
  geom_point(cex = 3) + facet_grid(.~classe) + scale_colour_gradient(low = "#132B41", high = "#56B1F9")


names(training)
trans<-preProcess(training[, -54], method = c("BoxCox", "center", "scale", "pca"))
trainPc<-predict(trans, training[,-54])
trainPc$classe<-training$classe

trans2<-preProcess(training[, -54], method = c("BoxCox"))
train2<-predict(trans2, training[,-54])
train2$classe<-training$classe
par(mfrow =c(1,2))
names(train2)
ggplot(train2, aes(x=magnet_arm_z, y=magnet_arm_x, colour = magnet_arm_y)) + 
  geom_point(cex = 3) + facet_grid(.~classe) + scale_colour_gradient(low = "#132B41", high = "#56B1F9")


typeColor<-training$classe
plot(trainPc[,1], trainPc[,2], col = typeColor)

ggplot(data = trainPc, aes(x = trainPc[,1], y = trainPc[, 2], colour = trainPc[, 3])) + 
  geom_point(cex = 3, aplha = 0.5) + facet_grid(.~classe)  
names(training)
str(trans)

install.packages("corrgram")
library(corrgram)
corrgram(training[, grep("")])

preProcBoxCox<-preProcess(training[,-58], method = "BoxCox")
plot(preProcBoxCox)

set.seed(5423)
fit<-train(classe ~ ., method = "rpart", data = training)

pred<-predict(fit, newdata = validation)
mean(validation$classe == pred)#0.5989809

fit<-train(classe ~ ., method = "rpart", trControl = trainControl(method = "cv", p = 0.6), data = training)
pred<-predict(fit, newdata = validation)
mean(validation$classe == pred)


head(training, n = 2)
dim(training)
names(training)
library(scales)
head(training)

training$classe<-as.factor(training$classe)

sum(is.na(training$max_roll_belt))

head(training)
with(training, cor(magnet_arm_x, pitch_arm))
qplot(magnet_arm_x, pitch_arm, colour = classe, data = training) + 
  geom_point(cex = 3) + colScale

qplot(roll_arm, yaw_arm, colour = classe, data = training) + geom_point(cex = 3)
ggplot(training, aes(x=roll_arm, y=yaw_arm)) + geom_point(cex = 3) + facet_grid(.~classe)

ggplot(training, aes(x=magnet_arm_x, y=yaw_arm)) + geom_point(cex = 3) + facet_grid(.~classe)
ggplot(training, aes(x=roll_arm, y=magnet_arm_z)) + geom_point(cex = 3) + facet_grid(.~classe)
qplot(x = classe, y = magnet_arm_z, data = training) + geom_violin()
ggplot(training, aes(x=pitch_arm, y=magnet_arm_x)) + geom_point(cex = 3) + facet_grid(.~classe)
ggplot(training, aes(x=roll_arm, y=magnet_arm_x)) + geom_point(cex = 3) + facet_grid(.~classe)
qplot(x = classe, y = magnet_arm_x, data = training) + geom_violin()
ggplot(training, aes(x=magnet_arm_z, y=magnet_arm_x, colour = magnet_arm_y)) + 
  geom_point(cex = 3) + facet_grid(.~classe) + scale_colour_gradient(low = "#132B41", high = "#56B1F9")

ggplot(training, aes(x=pitch_arm, y=accel_arm_x)) + geom_point() + facet_grid(.~classe)

ggplot(training, aes(x=gyros_arm_x, y=gyros_arm_y, colour = gyros_arm_z)) + 
  geom_point(cex = 3) + facet_grid(.~classe) + scale_colour_gradient(low = "#132B41", high = "#56B1F9")

qplot(x = accel_arm_x, y = gyros_arm_y, data = training) + geom_point(cex = 3) + facet_grid(.~classe)

library(dplyr)
product<-training %>% mutate(product = accel_arm_x*gyros_arm_y, classe=classe)
head(product)
qplot(x = classe, y = product, data = product) + geom_violin()


View(training[training$pitch_arm == 0[1], ])
View(woroll[1])
head(woroll)
qplot(x = accel_arm_x, y = roll_arm, data = training) + geom_point(cex = 3) + facet_grid(.~classe)
mean(training$roll_arm == 0 & training$yaw_arm == 0)
woroll<-training[training$roll_arm!= 0, ]
wroll<-training[training$roll_arm == 0, ]
plot(wroll$roll_arm, wroll$yaw_arm)
mean(woroll$pitch_arm == 0)
head(wroll)
View(wroll[1, ])

ggplot(training, aes(x=accel_arm_x, y=accel_arm_z, colour = accel_arm_y)) + 
  geom_point(cex = 3) + facet_grid(.~classe) + scale_colour_gradient(low = "#132B41", high = "#56B1F9")

ggplot(data = training, aes(x = pitch_arm, y = yaw_arm)) + 
  geom_point(cex = 3) + facet_grid(.~classe)
qplot(x = classe, y = magnet_arm_y, data = training) + geom_violin()

class(training$classe)
#investigar el belt
belt<-subset(training, select = c(grep("belt", names(training)), classe))
names(belt)
pairs(belt)

#el belt puede hablar de la clase 5, que corresponde a tirar las caderas para alante

names(training)
dumbbell<-subset(training, select = c(grep("dumbbell", names(training)), classe))
names(dumbbell)
qplot(accel_dumbbell_x, accel_dumbbell_y, data = training, colour = classe) + geom_point()
View(dumbbell[9274, ])
dumbbell$classe<-as.factor(dumbbell$classe)
pairs(dumbbell)


forearm<-subset(training, select = c(grep("forearm", names(training)), classe))
arm<-subset(training, select = c(grep("_arm", names(training)), classe))
pairs(arm[1:13])

head(forearm, n = 1)
qplot(x = total_accel_arm, data = training) + geom_bar() + facet_grid(classe~.)
qplot(x = total_accel_arm, y = total_accel_forearm, data = training) + geom_point() + facet_grid(.~classe)
names(forearm)
head(forearm)

pairs(forearm[, -c(4:12, 23)], col = as.factor(forearm$classe))

ggplot(training, aes(x=gyros_forearm_x, y=gyros_forearm_y, colour = gyros_forearm_z)) + 
  geom_point(cex = 3) + facet_grid(.~classe) + scale_colour_gradient(low = "#132B41", high = "#56B1F9")

ggplot(sgyros, aes(x=gyros_forearm_x, y=gyros_forearm_y, colour = gyros_forearm_z)) + 
  geom_point(cex = 3) + facet_grid(.~classe) + scale_colour_gradient(low = "#132B41", high = "#56B1F9")

ggplot(training, aes(x = gyros_forearm_y, y = gyros_arm_y)) + geom_point(cex = 3) + facet_grid(.~classe)
mgyros<-training%>%mutate(ygyros = gyros_forearm_y + gyros_arm_y)
ggplot(mgyros, aes(x = ygyros)) + geom_bar() + facet_grid(classe~.)
qplot(y = ygyros, colour = classe, data = mgyros) + geom_point(cex=3)
training$classe<-as.factor(training$classe)
gyrosdata<-subset(training, select = c(grep("gyros", names(training)), classe))
pairs(gyrosdata[, -dim(gyrosdata)[2]], col =as.factor(gyrosdata$classe))
