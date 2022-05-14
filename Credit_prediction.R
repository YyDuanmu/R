#数据导入
loans<-read.csv("C:\\Users\\Laysher\\Desktop\\DM\\loans.csv",header=TRUE,stringsAsFactors=F)
accounts<-read.csv("C:\\Users\\Laysher\\Desktop\\DM\\accounts.csv",header=TRUE)
card<-read.csv("C:\\Users\\Laysher\\Desktop\\DM\\card.csv",header=TRUE,stringsAsFactors=F)
clients<-read.csv("C:\\Users\\Laysher\\Desktop\\DM\\clients.csv",header=TRUE,stringsAsFactors=F)
disp<-read.csv("C:\\Users\\Laysher\\Desktop\\DM\\disp.csv",header=TRUE,stringsAsFactors=F)
district<-read.csv("C:\\Users\\Laysher\\Desktop\\DM\\district.csv",header=TRUE,stringsAsFactors=F)
trans<-read.csv("C:\\Users\\Laysher\\Desktop\\DM\\trans.csv",header=TRUE,stringsAsFactors=F)
order<-read.csv("C:\\Users\\Laysher\\Desktop\\DM\\order.csv",header=TRUE,stringsAsFactors=F)

#数据处理
#数据类型转换
accounts$date<-as.Date(accounts$date)
card$issued<-as.Date(card$issued)
card$type<-as.factor(card$type)
clients$sex<-as.factor(clients$sex)
clients$birth_date<-as.Date(clients$birth_date)
disp$type<-as.factor(disp$type)
loans$date<-as.Date(loans$date)
loans$status<-as.factor(loans$status)
trans$date<-as.Date(trans$date)
#去除千分位和美元符号,然后转换成数值类型
library(stringr)
trans$amount<-gsub(",","",trans$amount)
trans$balance<-gsub(",","",trans$balance)
trans$amount<-as.numeric(str_sub(trans$amount,2,nchar(trans$amount)))
trans$balance<-as.numeric(str_sub(trans$balance,2,nchar(trans$balance)))

#构建被解释变量
head(loans)
str(loans)
loans$New_status[loans$status=='A']<-'0'
loans$New_status[loans$status=='B']<-'1'
loans$New_status[loans$status=='C']<-'2'
loans$New_status[loans$status=='D']<-'1'
loans$New_status<-as.factor(loans$New_status)

#构建自变量
#只有“所有者”才有权限进行贷款
data<-merge(loans,disp,by.x="account_id",by.y="account_id",all.x=TRUE)
data<-data[data$type=="所有者",]
data<-merge(data,clients,by.x="client_id",by.y="client_id",all.x=TRUE)
data<-merge(data,district,by.x="district_id",by.y="A1",all.x=TRUE)
head(data)
str(data)
#求交集
data_temp<-merge(loans,trans,by.x="account_id",by.y="account_id",all=FALSE)
str(data_temp)
#一年为窗口期来取交易行为数据，即保留贷款日期前365天至贷款前1天内的交易数据
data_temp<-data_temp[data_temp$date.x>data_temp$date.y&data_temp$date.x<data_temp$date.y+365,]
#计算每个贷款帐户贷款前一年的平均帐户余额（代表财富水平）、帐户余额的标准差（代表财富稳定情况）和变异系数（代表财富稳定情况的另一个指标）
mean<-aggregate(data_temp[,14], by = list(data_temp[,1]), mean)
sd<-aggregate(data_temp[,14], by = list(data_temp[,1]), sd)
names(mean)<-c("account_id","mean")
names(sd)<-c("account_id","sd")
data_temp1<-merge(mean,sd,by.x="account_id",by.y="account_id",all=TRUE)
data_temp1$cv<-data_temp1$sd/data_temp1$mean
head(data_temp1)
#计算平均入账和平均支出的比例。首先按照上一步时间窗口取数得到的数据集，按照每个帐户的“借-贷”类型分别汇总交易金额
amount<-aggregate(data_temp[,13], by = list(data_temp[,1],data_temp[,11]), sum)
names(amount)<-c("account_id","type","amount")
out<-amount[amount$type=="借",]
income<-amount[amount$type=="贷",]
names(out)<-c("account_id","type","out")
names(income)<-c("account_id","type","income")
data_temp2<-merge(income,out,by.x="account_id",by.y="account_id",all=TRUE)
#缺失值的处理，赋值0
data_temp2[is.na(data_temp2$out)==TRUE,5]<-0
data_temp2$r_out_in<-data_temp2$out/data_temp2$income
head(data_temp2)
#将计算平均帐户余额、帐户余额的标准差、变异系数、平均入账和平均支出的比例等变量与之前的data合并
data1<-merge(data,data_temp1,by.x="account_id",by.y="account_id",all=TRUE)
data1<-merge(data1,data_temp2,by.x="account_id",by.y="account_id",all=TRUE)
#计算贷存比、贷收比
data1$r_lb<-data1$amount/data1$mean
data1$r_lincome<-data1$amount/data1$income
#缺失值处理
#判断缺失值的个数
sapply(data1,function(x) sum(is.na(x)))
#缺失值作图
#install.packages("Amelia")
library(Amelia)
missmap(data1, main = "Missing values vs observed")
#缺失值用均值替代
data1$A12[is.na(data1$A12)] <- mean(data1$A12,na.rm=T)
data1$A15[is.na(data1$A15)] <- mean(data1$A15,na.rm=T)

#逻辑回归
#提取状态为C的用于预测。其它样本随机抽样，建立训练集与测试集
data2<-data1[,c(6,7,10,15,16,17,18,19,20,21,22,23,24,25,26,28,30,31,32,33)]
data_model<-data2[data2$New_status!=2,]
for_predict<-data2[data2$New_status==2,]
n<-nrow(data_model)
rnd<-sample(n,n*.70)
train<-data_model[rnd,]
test<-data_model[-rnd,]

#使用向前逐步法进行逻辑回归建模
formula<-New_status~GDP+A4+A10+A11+A12+amount+duration+A13+A14+A15+a16+mean+sd+cv+income+out+r_out_in+r_lb+r_lincome
model<-glm(formula,data=train,family = binomial(link=logit)) 
forward_model<-step(model,direction="forward")
summary(forward_model)
#向后法
backward_model<-step(model,direction="backward")
summary(backward_model)
#逐步回归
both_model<-step(model,direction="both")
summary(both_model)
forward_model<-step(model,direction="forward")
Start: ??AIC=155.4
New_status ~ GDP + A4 + A10 + A11 + A12 + amount + duration + 
  A13 + A14 + A15 + a16 + mean + sd + cv + income + out + r_out_in + 
  r_lb + r_lincome

summary(forward_model)

#用测试集做模型评估
pre<-predict(both_model,test,type="response")
#在预测数据集中，概率大于0.5,违约，概率小于0.5，不违约
test$pre_New_status<-ifelse(predict(both_model,test,type="response")>0.5,1,0)
table(test$New_status,test$pre_New_status)
#准确率计算
sum_diag<-sum(diag(table(test$New_status,test$pre_New_status)))
sum<-sum(table(test$New_status,test$pre_New_status))
accuracy<-sum_diag/sum
accuracy
#ROC曲线评估
library(pROC)
library(sjmisc)
roc_curve<-roc(test$New_status~pre)
x<-1-roc_curve$specificities
y<-roc_curve$sensitivities
plot(x=x,y=y,xlim=c(0,1),ylim=c(0,1),xlab = '1-specificity',ylab = 'Sensitivity',main='ROC Curve',type='l',lwd=5)
abline(a=0,b=1,col='gray')
auc<-roc_curve$auc
text(0.5,0.4,paste('AUC:',round(auc,digits = 2)),col='blue')
#预测
for_predict$predict<-predict(both_model,for_predict,type="response")

library(randomForest)
rf_model <- randomForest(New_status~.,data=data2)
plot(rf_model, ylim=c(0,1))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
library(dplyr)
train1=train %>% mutate_if(is.character, as.factor)