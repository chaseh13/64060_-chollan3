library(caret)
library(e1071)
library(class)
library(tidyverse)

DF <- read.csv("C:/Users/choll/Desktop/UniversalBank.csv")

DF$Online <- as.factor(DF$Online)
is.factor(DF$Online)
DF$CreditCard <- as.factor(DF$CreditCard)
is.factor(DF$CreditCard)
DF$Personal.Loan <- as.factor(DF$Personal.Loan)
is.factor(DF$Personal.Loan)

set.seed(123)

DF1 <- createDataPartition(DF$Personal.Loan, p=0.60, list = FALSE)
training <- DF[DF1,]
validation <- DF[-DF1,]

norm_data <- preProcess(training[,-c(10,13,14)], method = c("center", "scale"))
predict_tdata <- predict(norm_data, training)
predict_vdata <- predict(norm_data, validation)

##A
pivottable<- ftable(predict_tdata$Personal.Loan, predict_tdata$Online, predict_tdata$CreditCard, dnn=c('Personal.loan','CreditCard', 'Online'))
pivottable

##B
prob.cust<-pivottable[4,2]/(pivottable[2,2]+pivottable[4,2])
prob.cust

##C
pivottable1<- ftable(predict_tdata$Personal.Loan,predict_tdata$Online,dnn=c('Personal.loan','Online'))
pivottable1

pivottable2<- ftable(predict_tdata$Personal.Loan,predict_tdata$CreditCard, dnn=c('Personal.loan','CreditCard'))
pivottable2

##D
prob.D1<- pivottable2[2,2]/(pivottable2[2,2]+pivottable2[2,1])
prob.D1

prob.D2 <- pivottable1[2,2]/(pivottable1[2,2]+pivottable1[2,1])
prob.D2

prob.D3 <- ftable(predict_tdata[,10])
prob.D3
prob.D3.2 <- prob.D3[1,2]/(prob.D3[1,2]+prob.D3[1,1])
prob.D3.2

prob.D4 <- pivottable2[1,2]/(pivottable2[1,2]+pivottable2[1,1])
prob.D4

prob.D5 <- pivottable1[1,2]/(pivottable1[1,2]+pivottable1[1,1])
prob.D5

prob.D6 <- ftable(predict_tdata[,10])
prob.D6
prob.D6.2 <- prob.D6[1,1]/(prob.D6[1,1]+prob.D6[1,2])
prob.D6.2    

##E
nb <- (prob.D1*prob.D2*prob.D3.2)/(prob.D1*prob.D2*prob.D3.2+prob.D4*prob.D5*prob.D6.2)
nb

##F


##G
naivebayes <-naiveBayes(Personal.Loan~Online+CreditCard, data = predict_tdata)
naivebayes


