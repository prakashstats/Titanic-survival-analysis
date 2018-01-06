# // Titanic dataset problem

rm(list=ls())
train<-read.csv(file.choose())  # Import train dataet for model fitting
head(train)
attach(train);str(train)

# // Check missing values in a variable before going to model fitting
which(is.na(Age)=="TRUE")
which(Embarked==""); str(Embarked)    
sex=as.numeric(train$Sex)
embarked<-as.numeric(Embarked)  # Replace missing value of embarked by their median
embarked1=embarked[-which(embarked==1)]
for(i in 1:length(embarked)){
if(embarked[i]==1){
  embarked[i]=median(embarked1)}}

male.sex<-which(Sex=="male")    
male.age<-Age[male.sex]
male.md<-median(Age[male.sex],na.rm=T)   # male.md is the median of male whose age is given

female.sex<-Sex[which(Sex=="female")]
female.age<-Age[female.sex]
female.md<-median(Age[female.sex],na.rm=T)  # female.md is the median of female whose age is given

# // Replace missing values of age corresponding to median of male & female
for(i in 1:length(Age)){
if(is.na(Age[i])==TRUE  && Sex[i]=="male") {
  Age[i]=male.md } else {
if(is.na(Age[i]==TRUE  && Sex[i]=="female")) {
  Age[i]=female.md }}}

test<-read.csv(file.choose())   # Import test dataset for model validation
test1=test[,-c(1,3,8,10)]       # Remove the variable which is not relevant

for(i in 1:length(test1$Age)){
if(is.na(test1$Age[i])==TRUE && test1$Sex[i]=="male"){ test1$Age[i]=male.md } else {
if(is.na(test1$Age[i])==TRUE && test1$Sex[i]=="female"){ test1$Age[i]=female.md }}}

test1$Fare[153]<-mean(train$Fare,na.rm=T)   # since Fare variable of 153 position are missing


# // Fitting Logistic regression model
model<-glm(Survived~Sex+Embarked+Pclass+Parch+Age+Fare+SibSp,data=train,family="binomial")
summary(model)
Survived<-as.numeric(predict(model,test1,type="response")>0.6)
write.csv(data.frame(PassengerId=892:1309,Survived),file.choose())
