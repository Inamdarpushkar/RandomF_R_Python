install.packages("MASS")
install.packages("randomForest")

library(caTools)
library(randomForest)
library(MASS)

set.seed(123)

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











