#RanfomFrorest
install.packages("MASS")
install.packages("randomForest")
install.packages("pROC")

library(caTools)
library(randomForest)
library(MASS)
library(pROC)


caset.seed(123)

df<-birthwt

# To check NA's
colSums(is.na(df))

#To check categorical variables
#low unique count in large dataset generally means categorical variable
apply(df,2,function(x) length(unique(x)))

hist(df$age)

#categorical casting
#it is important because RF works differently with nummeric and categorical data
# in case of numerical values RF first sorts the values and then finds best fit while 
#incase of categorical data it finds 2^N-2 possible combinations

cols<-c("low","race","smoke","ptl","ht","ui","ftv")
for (i in cols){
  df[,i]=as.factor(df[,i])
}

ind<-sample.split(Y=df$low,SplitRatio = 0.7)
traindf<-df[ind,]
testind<-df[!ind,]

modelRandom<-randomForest(low~.,data=traindf,mtry=3,ntree=20)

#paramteres
#mtry=number of variables selected at each split
# In case of regression, mtry is number of variables/3
# In case of categorical variables, mtry is number of variables
# square root of number of independent variables

#lower mtry (size of mtry)
#-less correlation between trees
#-decreases strength of each tree

#ntree=no. of trees to grow
#nodsize=minimum size of terminal nodes


### Forest error rate
#OOB (Out of bag error-misclassification rate)
#Each tree is tested on 1/3rd of the no. the observations
# that means 2/3 of samples used to build a model
# this is default value

# high strength of tree will have lower error [depends on mtry]
# high correlation between trees increases the error [depends on mtry]


#check important variables
importance(modelRandom)
varImpPlot(modelRandom)

#Predictions 
PredictionsWithClass<-predict(modelRandom,testind,type = 'class')
t<-table(predictions=PredictionsWithClass,actual=testind$low)

#accuracy
sum(diag(t)/sum(t))

#ROC
PredictionsWithProbs<-predict(modelRandom,testind,type = 'prob')
auc<-auc(testind$low,PredictionsWithProbs[,2])
plot(roc(testind$low,PredictionsWithProbs[,2]))

#find best mtry
bestmtry<-tuneRF(traindf,traindf$low,ntreeTry = 20,stepFactor = 1.2,improve = 0.01, trace = TRUE, plot = TRUE)

#One of the drawbacks of randomforest is if certain catagorical
#variable has many levels then random forest is biased towards using this variable
# in trees










