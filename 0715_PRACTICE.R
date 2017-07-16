
#預測數值
# Iris Multiple Regression
model=lm(Sepal.Length ~ . ,data=iris)
summary(step(model))
irisnew=read.csv("d:\\R\\iris_new.txt", header=T)
predict(model,newdata=irisnew, level=0.95, interval="confidence")
#原4.8 5.7 5.0 7.0 6.3 4.9 6.3 7.6 6.5 5.9

install.packages('xlsx')
install.packages("xlsx")

boston=read.csv("d:\\R\\Boston_Housing.csv", header=T)
head(boston)

model=lm(MEDV ~ . ,data=boston)  #MEDV要預測

summary(step(model)) #副回歸模型常用step變數篩選
bostonnew=read.csv("d:\\R\\New_Boston_Housing.csv", header=T)
predict(model,newdata=bostonnew, level=0.95, interval="confidence")

library()
library(car)
#模型不是很好，殘差不符和
#殘差分析
# Residuals and Variance Inflation Factor
par(mai=c(0.5,0.5,0.5,0.5),mfrow=c(2,2))
plot(model)
resid=model$residuals
shapiro.test(resid)

ncvTest(model)
durbinWatsonTest(model)
vif(model)
mean(vif(model))


# Outliers
hii=hatvalues(model)
h=2*ncol(boston)/nrow(boston)
#槓桿值若大於2p/n ,則可能是離群值或影響點,p是含截距的自變項個數
(which(as.vector(hii)>h))
influencePlot(model)  #369 381離群

#去除369 381 重作model
boston=read.csv("d:\\R\\Boston_Housing.csv", header=T)
boston=boston[-c(369,381)]
head(boston)
model=lm(MEDV ~ . ,data=boston)  #MEDV要預測
summary(step(model)) #副回歸模型常用step變數篩選
bostonnew=read.csv("d:\\R\\New_Boston_Housing.csv", header=T)
predict(model,newdata=bostonnew, level=0.95, interval="confidence")
influencePlot(model)





cooks.distance(model)
which(as.vector(cooks.distance(model))>1)
(q=qf(0.5,7,23-7)) #F0.5(p,n-p)
which(as.vector(cooks.distance(model))>q)
(student.residual=summary(model)$residuals/(summary(model)$sigma*sqrt(1-hii)))
influencePlot(model)


install.packages("multcomp")
library(multcomp)
install.packages("laercio")
library(laercio)
install.packages("asbio")
library(asbio)
#變異數分析
attach(iris)
model=lm(Sepal.Length ~ . ,data=iris)  
summary(model)
summary(glht(model,linfct=mcp(Species="Tukey")))
LTukey(aov(Sepal.Width~Species),"Species")
      

#多重比較

etch=read.table("d:\\R\\etch.txt", header=T)
attach(etch)
model=lm(Y ~ A ,data=etch)  
summary(model)
summary(glht(model,linfct=mcp(A="Tukey")))
LTukey(aov(Y~A),"A")
LDuncan(aov(Y~A),"A")
library(asbio)
tukeyCI(Y,A)



#二元羅吉斯迴歸程式碼

# babies (if bwt<25% then bwt2=1 else bwt2=0)
babies=read.table("d:\\R\\babies.txt",header=T)
babies=na.exclude(babies)
babies$bwt2=(babies$bwt<quantile(babies$bwt,0.25))*1 
babies$bwt2=as.factor(babies$bwt2)  #重點 轉換成因子變數
head(babies)
summary(babies)
#90% sample as train group(index=1), 10% sample as test group(index=2)





p=0.7
index=sample(2,nrow(babies),replace=T,prob=c(p,1-p))
babies.train=babies[index==1,]

babies.test=babies[index==2,]
nrow(babies.test)
nrow(babies.train)
#glm function
train.result=glm(bwt2~gestation+parity+age+height+weight+smoke,data=babies.train,family=binomial(link=logit))
summary(train.result)
exp(train.result$coef) #exp(參數估計值)計算 odds ratio
confint(train.result) #參數的 95%信賴區間
exp(confint(train.result)) #exp(參數)的95%信賴區間
#Wald and Likelihood-Ration檢定
#library(epicalc)
logistic.display(train.result)

#訓練組混淆矩陣
#train confusion matrix
pred=predict(train.result,newdata=babies.train,type="response")
pred=round(pred)
tab=table(Y=babies.train$bwt2,Ypred=pred)
#rownames(tab)=levels(babies.train$bwt2) #可省略
#colnames(tab)=levels(babies.train$bwt2) #可省略
tab
cat("Total records(train)=",nrow(babies.train),"\n")
cat("Correct Classification Ratio(train)=",sum(diag(tab))/sum(tab)*100,"%\n")

correct.train=sum(diag(tab))/sum(tab)*100
trainv=c(trainv,correct,train)
#test confusion matrix
pred=predict(train.result,newdata=babies.test,type="response")
pred=round(pred)
tab=table(Y=babies.test$bwt2,Ypred=pred)
#rownames(tab)=levels(babies.train$bwt2) #可省略
#colnames(tab)=levels(babies.train$bwt2) #可省略
tab
cat("Total records(test)=",nrow(babies.test),"\n")
cat("Correct Classification Ratio(train)=",sum(diag(tab))/sum(tab)*100,"%\n")

correct.test=sum(diag(tab))/sum(tab)*100
testv=c(testv,correct,test)

#迴圈10次
install.packages("epicalc")

trainv=NULL
testv=NULL
for (i in 1:10)
{
  p=0.7
  index=sample(2,nrow(babies),replace=T,prob=c(p,1-p))
  babies.train=babies[index==1,]
  babies.test=babies[index==2,]
 # nrow(babies.test)
 # nrow(babies.train)
  #glm function
  train.result=glm(bwt2~gestation+parity+age+height+weight+smoke,data=babies.train,family=binomial(link=logit))
  #summary(train.result)
  #exp(train.result$coef) #exp(參數估計值)計算 odds ratio
  #confint(train.result) #參數的 95%信賴區間
  #exp(confint(train.result)) #exp(參數)的95%信賴區間
  #Wald and Likelihood-Ration檢定
  #library(epicalc)
  #logistic.display(train.result)
  
  #訓練組混淆矩陣
  #train confusion matrix
  pred=predict(train.result,newdata=babies.train,type="response")
  pred=round(pred)
  tab=table(Y=babies.train$bwt2,Ypred=pred)
  #rownames(tab)=levels(babies.train$bwt2) #可省略
  #colnames(tab)=levels(babies.train$bwt2) #可省略
  #tab
#  cat("Total records(train)=",nrow(babies.train),"\n")
 # cat("Correct Classification Ratio(train)=",sum(diag(tab))/sum(tab)*100,"%\n")
  
  correct.train=sum(diag(tab))/sum(tab)*100
  trainv=c(trainv,correct,train)
  #test confusion matrix
  pred=predict(train.result,newdata=babies.test,type="response")
  pred=round(pred)
  tab=table(Y=babies.test$bwt2,Ypred=pred)
  #rownames(tab)=levels(babies.train$bwt2) #可省略
  #colnames(tab)=levels(babies.train$bwt2) #可省略
  #tab
 # cat("Total records(test)=",nrow(babies.test),"\n")
 # cat("Correct Classification Ratio(train)=",sum(diag(tab))/sum(tab)*100,"%\n")
  
  correct.test=sum(diag(tab))/sum(tab)*100
  testv=c(testv,correct,test)
}
#trainv
#testv
mean(trainv)
mean(testv)



library(nnet)




#混淆矩陣及預測程式碼
p=0.9
index=sample(2,nrow(iris),replace=T, prob=c(p,1-p))
iris.train=iris[index==1,]
iris.test=iris[index==2,]
#nnet library multinom function
library(nnet)
train.result=multinom(Species~ .  , data=iris.train)
train.result
#train confusion matrix
Y.pred=predict(train.result,iris.train[, -5])
(tab=table(iris.train$Species,Y.pred))
cat("Correct Classification Ratio(train)=", sum(diag(tab))/sum(tab)*100,"%\n")
#test confusion matrix
Y.pred=predict(train.result,iris.test[, -5])
(tab=table(iris.test$Species,Y.pred))
cat("Correct Classification Ratio(test)=",sum(diag(tab))/sum(tab)*100,"%\n")
#new data prediction
irisnew2=read.csv("d:\\R\\iris_new2.txt",header=T)
(Y.pred=predict(train.result,irisnew2))
#以第150朵花為例
train.result
iris[150,]
iris150=c(1,5.9,3,5.1,1.8)
(beta.versicolor=c(18.02826,-6.899766,-7.981207,17.28076,-4.555761))
(beta.virginica=c( -26.91099,-8.362751,-13.544018,25.58417,12.981154))
(exp1=exp(iris150 %*% beta.versicolor)) #setosa vs versicolor的機率比例
(exp2=exp(iris150 %*% beta.virginica)) #setosa vs virginica的機率比例
(exp1/exp2) #versicolor vs virginica的機率比例
# 預測為各品種的機率
(setosa=1/(1+exp1+exp2))
(versicolor=exp1/(1+exp1+exp2))
(virginica=exp2/(1+exp1+exp2))

#失敗
# installed.packages("vcd")
# library(vcd)
# head(Arthritis)
# summary(Arthritis)

#順序型羅吉斯迴歸程式碼

art=read.table("d:\\R\\Arthritis.txt",header=T)
summary(art)
#Ordinal logistic
library(MASS)
result=polr(Improved~Treatment+Sex+Age, data=art,Hess=T)
result
summary(result)

#預測及計算分類機率程式碼
#predict and analyze
Pred=predict(result,newdata=art)
(tab=table(art$Improved,Pred))
(beta=result$coeff)
exp(beta)
(new3=art[c(23,48,66),])
predict(result,newdata=new3,type="probs")
predict(result,newdata=new3,type="class") # type="class"可省


#p.40決策樹run
#建立模型步驟1, 2, 3(刪id變項)
library(rpart)
wdbc=read.table("d:\\stella\\R\\wdbc.txt", header=T,sep=",")
wdbc=na.exclude(wdbc)
wdbc=wdbc[,-1]
#test group(test.index sampling 30% records), remaining 70% as train group 
n=0.3*nrow(wdbc)
test.index=sample(1:nrow(wdbc),n)
wdbc.train=wdbc[-test.index,]
wdbc.test=wdbc[test.index,]

#建立模型步驟4,5,6(用tree函數)
#decision tree
install.packages("tree")
library(tree)
wdbc.tree=tree(diagnosis~.,data=wdbc.train)
wdbc.tree
summary(wdbc.tree)
plot(wdbc.tree)
text(wdbc.tree)
#建立模型步驟7, 8(訓練組)
#train confusion matrix
diagnosis.train=wdbc$diagnosis[-test.index]
train.pred=predict(wdbc.tree,newdata= wdbc.train, type='class')
table.train=table(diagnosis.train,train.pred)
table.train
cat("Total records(train)=",nrow(wdbc.train), "\n")
cat("Correct Classification Ratio(train)=", sum (diag(table.train))/sum(table.train)*100,"%\n")
#建立模型7, 8(測試組)
#test confusion matrix
diagnosis.test=wdbc$diagnosis[test.index]
test.pred=predict(wdbc.tree,newdata=wdbc.test, type='class')
table.test=table(diagnosis.test,test.pred)
table.test
cat("Total records(test)=",nrow(wdbc.test),"\n")
cat("Correct Classification Ratio(test)=", sum(diag(table.test))/sum(table.test)*100,"%\n")


