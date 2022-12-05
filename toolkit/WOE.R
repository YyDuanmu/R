cor1 <- cor(fvfv[,1:6])
corrplot(cor1)
corrplot(cor1,method = "number")
table(fvfv$categories)

set.seed(100)
library(caret)
train <- fvfv[splitIndex,]
test <- fvfv[-splitIndex,]
prop.table(table(train$categories))

prop.table(table(test$categories))

fit <- glm(categories~.,train,family = "biomial")
summary(fit)

pre <- predict(fit,test)

library(pROC)
modelroc <- roc(test$categories,pre)
plot(modelroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)

cutAr1=c(-Inf,10,20,30,40,50,60,Inf)
plot(cut(train$Ar1,cutAr1))

cutT1 <- c(-Inf,100000,200000,300000,400000,500000,600000,Inf)
plot(cut(train$T1,cutT1))

cutC1=c(-Inf,1500,3000,4500,6000,7500,Inf)
plot(cut(train$C1,cutC1))

cutAn1=c(-Inf,0.8,3,8,15,40,100,Inf)
plot(cut(train$An1,cutAn1))

cutL1=c(-Inf,0.5,1.2,1.6,2.5,8,15,Inf)
plot(cut(train$L1,cutL1))

totalgood = as.numeric(table(train$categories))[1]
totalbad = as.numeric(table(train$categories))[2]

getWOE <- function(a,p,q)
{
  Good <- as.numeric(table(train$categories[a > p & a <= q]))[1]
  Bad <- as.numeric(table(train$categories[a > p & a <= q]))[2]
  WOE <- log((Bad/totalbad)/(Good/totalgood),base = exp(1))
  return(WOE)
}


Arlessthan10.WOE=getWOE(train$Ar1,-Inf,10)
Ar10to20.WOE=getWOE(train$Ar1,10,20)
Ar20to30.WOE=getWOE(train$Ar1,20,30)
Ar30to40.WOE=getWOE(train$Ar1,30,40)
Ar40to50.WOE=getWOE(train$Ar1,40,50)
Ar50to60.WOE=getWOE(train$Ar1,50,60)
Armorethan60.WOE=getWOE(train$Ar1,60,Inf)
ar.WOE=c(Arlessthan10.WOE,Ar10to20.WOE,Ar20to30.WOE,Ar30to40.WOE,Ar40to50.WOE,Ar50to60.WOE,Armorethan60.WOE)


Tlessthan100000.WOE=getWOE(train$T1,-Inf,100000)
T100000to200000.WOE=getWOE(train$T1,100000,200000)
T200000to300000.WOE=getWOE(train$T1,200000,300000)
T300000to400000.WOE=getWOE(train$T1,300000,400000)
T400000to500000.WOE=getWOE(train$T1,400000,500000)
T500000to600000.WOE=getWOE(train$T1,500000,600000)
Tmorethan600000.WOE=getWOE(train$T1,600000,Inf)
t.WOE=c(Tlessthan100000.WOE,T100000to200000.WOE,T200000to300000.WOE,T300000to400000.WOE, T400000to500000.WOE ,T500000to600000.WOE,Tmorethan600000.WOE)

Clessthan1500.WOE=getWOE(train$C1,-Inf,1500)
C1500to3000.WOE=getWOE(train$C1,1500,3000)
C3000to4500.WOE=getWOE(train$C1,3000,4500)
C4500to6000.WOE=getWOE(train$C1,4500,6000)
C6000to7500.WOE=getWOE(train$C1,6000,7500)
Cmorethan7500.WOE=getWOE(train$C1,7500,Inf)
C.WOE=c(Clessthan1500.WOE,C1500to3000.WOE,C3000to4500.WOE,C4500to6000.WOE,C6000to7500.WOE,Cmorethan7500.WOE)

Anlessthan0.8.WOE=getWOE(train$An1,-Inf,0.8)
An0.8to3.WOE=getWOE(train$An1,0.8,3)
An3to8.WOE=getWOE(train$An1,3,8)
An8to15.WOE=getWOE(train$An1,8,15)
An15to40.WOE=getWOE(train$An1,15,40)
An40to100.WOE=getWOE(train$An1,40,100)
Anmorethan100.WOE=getWOE(train$An1,100,Inf)
an.WOE=c(Anlessthan0.8.WOE,An0.8to3.WOE,An3to8.WOE,An8to15.WOE,An15to40.WOE,An40to100.WOE,Anmorethan100.WOE)

Llessthan0.5.WOE=getWOE(train$L1,-Inf,0.5)
L0.5to1.2.WOE=getWOE(train$L1,0.5,1.2)
L1.2to1.6.WOE=getWOE(train$L1,1.2,1.6)
L1.6to2.5.WOE=getWOE(train$L1,1.6,2.5)
L2.5to8.WOE=getWOE(train$L1,2.5,8)
L8to15.WOE=getWOE(train$L1,8,15)
Lmorethan15.WOE=getWOE(train$L1,15,Inf)
l.WOE=c(Llessthan0.5.WOE,L0.5to1.2.WOE,L1.2to1.6.WOE,L1.6to2.5.WOE, L2.5to8.WOE ,L8to15.WOE,Lmorethan15.WOE)

tmp.ar <- 0
for(i in 1:nrow(train)) {
  if(train$Ar1[i] <= 10)
    tmp.ar[i] <- Arlessthan10.WOE
  else if(train$Ar1[i] <= 20)
    tmp.ar[i] <- Ar10to20.WOE
  else if(train$Ar1[i] <= 30)
    tmp.ar[i] <- Ar20to30.WOE
  else if(train$Ar1[i] <= 40)
    tmp.ar[i] <- Ar30to40.WOE
  else if(train$Ar1[i] <= 50)
    tmp.ar[i] <- Ar40to50.WOE
  else if(train$Ar1[i] <= 60)
    tmp.ar[i] <- Ar50to60.WOE
  else
    tmp.ar[i] <- Armorethan60.WOE
}
table(tmp.ar)
train$Ar1[1:10]

tmp.t <- 0
for(i in 1:nrow(train)) {
  if(train$T1[i] <= 100000)
    tmp.t[i] <- Tlessthan100000.WOE
  else if(train$T1[i] <= 200000)
    tmp.t[i] <- T100000to200000.WOE
  else if(train$T1[i] <= 300000)
    tmp.t[i] <- T200000to300000.WOE
  else if(train$T1[i] <= 400000)
    tmp.t[i] <- T300000to400000.WOE
  else if(train$T1[i] <= 500000)
    tmp.t[i] <- T400000to500000.WOE
  else if(train$T1[i] <= 600000)
    tmp.t[i] <- T500000to600000.WOE
  else
    tmp.t[i] <- Tmorethan600000.WOE
}
table(tmp.t)
train$T1[1:10]


tmp.C <- 0
for(i in 1:nrow(train)) {
  if(train$C1[i] <= 1500)
    tmp.C[i] <- Clessthan1500.WOE
  else if(train$C1[i] <= 3000)
    tmp.C[i] <- C1500to3000.WOE
  else if(train$C1[i] <= 4500)
    tmp.C[i] <- C3000to4500.WOE
  else if(train$C1[i] <= 6000)
    tmp.C[i] <- C4500to6000.WOE
  else if(train$C1[i] <= 7500)
    tmp.C[i] <- C6000to7500.WOE
  else
    tmp.C[i] <- Cmorethan7500.WOE
}
table(tmp.C)
train$C1[1:10]

tmp.an <- 0
for(i in 1:nrow(train)) {
  if(train$An1[i] <= 0.8)
    tmp.an[i] <- Anlessthan0.8.WOE
  else if(train$An1[i] <= 3)
    tmp.an[i] <- An0.8to3.WOE
  else if(train$An1[i] <= 8)
    tmp.an[i] <- An3to8.WOE
  else if(train$An1[i] <= 15)
    tmp.an[i] <- An8to15.WOE
  else if(train$An1[i] <= 40)
    tmp.an[i] <- An15to40.WOE
  else if(train$An1[i] <= 100)
    tmp.an[i] <- An40to100.WOE
  else
    tmp.an[i] <- Anmorethan100.WOE
}
table(tmp.an)
train$An1[1:10]

tmp.l <- 0
for(i in 1:nrow(train)) {
  if(train$L1[i] <= 0.5)
    tmp.l[i] <- Llessthan0.5.WOE
  else if(train$L1[i] <= 1.2)
    tmp.l[i] <- L0.5to1.2.WOE
  else if(train$L1[i] <= 1.6)
    tmp.l[i] <- L1.2to1.6.WOE
  else if(train$L1[i] <= 2.5)
    tmp.l[i] <- L1.6to2.5.WOE
  else if(train$L1[i] <= 8)
    tmp.l[i] <- L2.5to8.WOE
  else if(train$L1[i] <= 15)
    tmp.l[i] <- L8to15.WOE
  else
    tmp.l[i] <- Lmorethan15.WOE
}
table(tmp.l)
train$L1[1:10]


trainWOE=cbind.data.frame(tmp.ar,tmp.t,tmp.C,tmp.an,tmp.l)

trainWOE$categories = 1-train$categories
glm.fit = glm(categories~.,data = trainWOE,family = binomial(link = logit)) 
summary(glm.fit) 
coe = (glm.fit$coefficients)

p <- 20/log(2)
q <- 600-20*log(15)/log(2)


Score=q + p*{as.numeric(coe[1])+as.numeric(coe[2])*tmp.ar +as.numeric(coe[3])*tmp.t+p*as.numeric(coe[4])*tmp.C+p*as.numeric(coe[5])*tmp.an+p*as.numeric(coe[6])*tmp.l}

base <- q + p*as.numeric(coe[1]) 
base

Arlessthan10.SCORE = p*as.numeric(coe[2])*Arlessthan10.WOE 
Ar10to20.SCORE = p*as.numeric(coe[2])*Ar10to20.WOE 
Ar20to30.SCORE = p*as.numeric(coe[2])*Ar20to30.WOE 
Ar30to40.SCORE = p*as.numeric(coe[2])*Ar30to40.WOE 
Ar40to50.SCORE = p*as.numeric(coe[2])*Ar40to50.WOE 
Ar50to60.SCORE = p*as.numeric(coe[2])*Ar50to60.WOE 
Armorethan60.SCORE=p*as.numeric(coe[2])*Armorethan60.WOE 
Ar.SCORE=c(Ar10to20.SCORE,Ar20to30.SCORE,Ar30to40.SCORE,Ar40to50.SCORE,Ar50to60.SCORE ,Armorethan60.SCORE) 
Ar.SCORE

Tlessthan100000.SCORE = p*as.numeric(coe[3])*Tlessthan100000.WOE 
T100000to200000.SCORE = p*as.numeric(coe[3])*T100000to200000.WOE 
T200000to300000.SCORE = p*as.numeric(coe[3])*T200000to300000.WOE 
T300000to400000.SCORE = p*as.numeric(coe[3])*T300000to400000.WOE 
T400000to500000.SCORE = p*as.numeric(coe[3])*T400000to500000.WOE 
T500000to600000.SCORE = p*as.numeric(coe[3])*T500000to600000.WOE 
Tmorethan600000.SCORE=p*as.numeric(coe[3])*Tmorethan600000.WOE 
T.SCORE=c(T100000to200000.SCORE,T200000to300000.SCORE,T300000to400000.SCORE,T400000to500000.SCORE,T500000to600000.SCORE ,Tmorethan600000.SCORE) 
T.SCORE


Clessthan1500.SCORE = p*as.numeric(coe[4])*Clessthan1500.WOE 
C1500to3000.SCORE = p*as.numeric(coe[4])*C1500to3000.WOE 
C3000to4500.SCORE = p*as.numeric(coe[4])*C3000to4500.WOE 
C4500to6000.SCORE = p*as.numeric(coe[4])*C4500to6000.WOE 
C6000to7500.SCORE = p*as.numeric(coe[4])*C6000to7500.WOE 
Cmorethan7500.SCORE=p*as.numeric(coe[4])*Cmorethan7500.WOE 
C.SCORE=c(C1500to3000.SCORE,C3000to4500.SCORE,C4500to6000.SCORE,C6000to7500.SCORE, Cmorethan7500.SCORE) 
C.SCORE

Anlessthan0.8.SCORE = p*as.numeric(coe[5])*Anlessthan0.8.WOE 
An0.8to3.SCORE = p*as.numeric(coe[5])*An0.8to3.WOE 
An3to8.SCORE = p*as.numeric(coe[5])*An3to8.WOE 
An8to15.SCORE = p*as.numeric(coe[5])*An8to15.WOE 
An15to40.SCORE = p*as.numeric(coe[5])*An15to40.WOE 
An40to100.SCORE = p*as.numeric(coe[5])*An40to100.WOE 
Anmorethan100.SCORE=p*as.numeric(coe[5])*Anmorethan100.WOE 
An.SCORE=c(An0.8to3.SCORE,An3to8.SCORE,An8to15.SCORE,An15to40.SCORE, An40to100.SCORE, Anmorethan100.SCORE) 
An.SCORE

Llessthan0.5.SCORE = p*as.numeric(coe[6])*Llessthan0.5.WOE 
L0.5to1.2.SCORE = p*as.numeric(coe[6])*L0.5to1.2.WOE 
L1.2to1.6.SCORE = p*as.numeric(coe[6])*L1.2to1.6.WOE 
L1.6to2.5.SCORE = p*as.numeric(coe[6])*L1.6to2.5.WOE 
L2.5to8.SCORE = p*as.numeric(coe[6])*L2.5to8.WOE 
L8to15.SCORE = p*as.numeric(coe[6])*L8to15.WOE 
Lmorethan15.SCORE=p*as.numeric(coe[6])*Lmorethan15.WOE 
L.SCORE=c(L0.5to1.2.SCORE,L1.2to1.6.SCORE,L1.6to2.5.SCORE,L2.5to8.SCORE, L8to15.SCORE, Lmorethan15.SCORE) 
L.SCORE


getscore<-function(i,x){
  score = round(p*as.numeric(coe[i])*x,0)
  return(score)
}



Arlessthan10.SCORE = getscore(2,Arlessthan10.WOE)
Ar10to20.SCORE = getscore(2,Ar10to20.WOE)
Ar20to30.SCORE = getscore(2,Ar20to30.WOE)
Ar30to40.SCORE = getscore(2,Ar30to40.WOE)
Ar40to50.SCORE = getscore(2,Ar40to50.WOE)
Ar50to60.SCORE = getscore(2,Ar50to60.WOE)
Armorethan60.SCORE = getscore(2,Armorethan60.WOE)
Ar.SCORE=c(Arlessthan10.SCORE,Ar10to20.SCORE,Ar20to30.SCORE,Ar30to40.SCORE,Ar40to50.SCORE,Ar50to60.SCORE, Armorethan60.SCORE)
Ar.SCORE

Tlessthan100000.SCORE = getscore(3,Tlessthan100000.WOE)
T100000to200000.SCORE = getscore(3,T100000to200000.WOE)
T200000to300000.SCORE = getscore(3,T200000to300000.WOE)
T300000to400000.SCORE = getscore(3,T300000to400000.WOE)
T400000to500000.SCORE = getscore(3,T400000to500000.WOE)
T500000to600000.SCORE = getscore(3,T500000to600000.WOE)
Tmorethan600000.SCORE = getscore(3,Tmorethan600000.WOE)
T.SCORE=c(Tlessthan100000.SCORE,T100000to200000.SCORE,T200000to300000.SCORE,T300000to400000.SCORE,T400000to500000.SCORE,T500000to600000.SCORE, Tmorethan600000.SCORE)
T.SCORE

Clessthan1500.SCORE = getscore(4,Clessthan1500.WOE)
C1500to3000.SCORE = getscore(4,C1500to3000.WOE)
C3000to4500.SCORE = getscore(4,C3000to4500.WOE)
C4500to6000.SCORE = getscore(4,C4500to6000.WOE)
C6000to7500.SCORE = getscore(4,C6000to7500.WOE)
Cmorethan7500.SCORE = getscore(4,Cmorethan7500.WOE)
C.SCORE=c(Clessthan1500.SCORE,C1500to3000.SCORE,C3000to4500.SCORE,C4500to6000.SCORE,C6000to7500.SCORE, Cmorethan7500.SCORE)
C.SCORE

Anlessthan0.8.SCORE = getscore(5,Anlessthan0.8.WOE)
An0.8to3.SCORE = getscore(5,An0.8to3.WOE)
An3to8.SCORE = getscore(5,An3to8.WOE)
An8to15.SCORE = getscore(5,An8to15.WOE)
An15to40.SCORE = getscore(5,An15to40.WOE)
An40to100.SCORE = getscore(5,An40to100.WOE)
Anmorethan100.SCORE = getscore(5,Anmorethan100.WOE)
An.SCORE=c(Anlessthan0.8.SCORE,An0.8to3.SCORE,An3to8.SCORE,An8to15.SCORE,An15to40.SCORE,An40to100.SCORE, Anmorethan100.SCORE)
An.SCORE


Llessthan0.5.SCORE = getscore(6,Llessthan0.5.WOE)
L0.5to3.SCORE = getscore(6,L0.5to1.2.WOE)
L1.2to1.6.SCORE = getscore(6,L1.2to1.6.WOE)
L1.6to2.5.SCORE = getscore(6,L1.6to2.5.WOE)
L2.5to8.SCORE = getscore(6,L2.5to8.WOE)
L8to15.SCORE = getscore(6,L8to15.WOE)
Lmorethan15.SCORE = getscore(6,Lmorethan15.WOE)
L.SCORE=c(Llessthan0.5.SCORE,L0.5to1.2.SCORE,L1.2to1.6.SCORE,L1.6to2.5.SCORE,L2.5to8.SCORE,L8to15.SCORE, Lmorethan15.SCORE)
L.SCORE


