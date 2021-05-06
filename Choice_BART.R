set.seed(21)

library(BART)          # BART Model
library(bayesm)        # Hierarchical Bayesian Model
library(mlogit)       # Mixed Effects GLM
library(ISLR)          # Orange Juice Data
library(Ecdat)         # Multiple econometrics data
library(pROC)          # Multiclass ROC curve
library(nnet)          # Multinomial Logit

n_mcmc = 1000        # Number of posterior samples for BART

# Load Datasets from "mlogit"
##### Electricity #####
data("Electricity", package = "mlogit")
# Apply BART
ytrain = as.numeric(Electricity$choice)
xtrain = Electricity[,-c(1,2)]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

ytrain = as.numeric(Electricity$choice)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(choice ~ ., data = Electricity)
probtest = fit_multinom$fitted.values
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

# Apply monBART

fit_mbart = multi_monbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
#test.prob = apply(fit_mbart$prob.test,MARGIN=2,FUN=mean)
test.prob = fit_mbart$prob.test.mean
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

# Fit mlogit
Electr = mlogit.data(Electricity, id="id", choice="choice",
                      varying=3:26, shape="wide", sep="")
#Electr = mlogit.data(Electricity, choice="choice", shape="wide", sep="")
# Fit a mixed logit model
# taking into account the panel data structure.
# rpar: a named vector whose names are the random parameters and 
# values the distribution :
# 'n':normal, 'l':log-normal, 't':truncated normal, 'u':uniform,
fit_mlogit = mlogit(choice~pf+cl+loc+wk+tod+seas, Electr,
                rpar=c(pf='n', cl='n', loc='n', wk='n', tod='n', seas='n'),
                   R=100, halton=NA, print.level=0, panel=TRUE)
multiclass.roc(ytest, fit_mlogit$probabilities)

# Load Datasets from "ISLR"
data(OJ)
# Fit BART
ytrain = OJ$Purchase
ytrain = as.numeric(ytrain)-1
xtrain = OJ[,-1]
fit_bart = pbart(x.train = xtrain, y.train = ytrain, 
                 x.test = xtrain, ndpost = n_mcmc)

test.prob = apply(pnorm(fit_bart$yhat.test),MARGIN=2,FUN=mean)
library(cvAUC)
ytest = ytrain
auc = AUC(test.prob,ytest)

# Fit monotone BART
library(mBART)
# Change signs of covariates so that
# Every relationship is monotone increasing with Prob(Y=1) (sale of MM)
xtrain = xtrain[,-c(1,2,13,17)]
xtrain$PriceMM = -xtrain$PriceMM
xtrain$DiscCH = -xtrain$DiscCH
xtrain$SpecialCH = -xtrain$SpecialCH
xtrain$LoyalCH = -xtrain$LoyalCH
xtrain$SalePriceMM = -xtrain$SalePriceMM
xtrain$PriceDiff = -xtrain$PriceDiff
xtrain$PctDiscCH = -xtrain$PctDiscCH
xtrain$ListPriceDiff = -xtrain$ListPriceDiff

fit_monbart = mBART::monbart(x.train = xtrain, y.train = ytrain, ndpost = n_mcmc)
test.prob = apply(pnorm(fit_monbart$yhat.train),MARGIN=2,FUN=mean)
library(cvAUC)
ytest = ytrain
auc = AUC(test.prob,ytest)

fit_multi_monbart = multi_monbart(x.train = xtrain, y.train = ytrain, ndpost = n_mcmc)
test.prob = apply(pnorm(fit_multi_monbart$yhat.train),MARGIN=2,FUN=mean)
library(cvAUC)
ytest = ytrain
auc = AUC(test.prob,ytest)

# Fit mlogit
ytest = as.numeric(ytest)
OJ1 = OJ
OJ1$Purchase = OJ$Purchase
choices = levels(OJ1$Purchase)
for (i in 1:length(choices)){
  z = 3+i
  colnames(OJ1)[z] = paste("Price.",choices[i])
}
for (i in 1:length(choices)){
  z = 5+i
  colnames(OJ1)[z] = paste("Disc.",choices[i])
}
for (i in 1:length(choices)){
  z = 7+i
  colnames(OJ1)[z] = paste("Special.",choices[i])
}
for (i in 1:length(choices)){
  z = 10+i
  colnames(OJ1)[z] = paste("SalePrice.",choices[i])
}
for (i in 1:length(choices)){
  z = 14+i
  colnames(OJ1)[z] = paste("PctDisc.",choices[i])
}
Data_mlogit = mlogit.data(OJ1, shape="wide", choice="Purchase", varying=c(4:9, 11:12, 15:16))
fit_mlogit = mlogit(Purchase~Price, Data_mlogit)
auc(ytest,fit_mlogit$probabilities[,1])


ytrain = as.numeric(OJ$Purchase)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(Purchase ~ ., data = OJ)
probtest = fit_multinom$fitted.values
auc(ytest,probtest)

###################################################

# Load Datasets from "Ecdat"
library(Ecdat)

##### Yogurt #####
data(Yogurt)
# Fit BART
ytrain = as.numeric(Yogurt$choice)
xtrain = Yogurt[,-10]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)
# Fit mlogit
Data_mlogit = mlogit.data(Yogurt, shape="wide", id = "id", choice="choice", varying=c(2:9))
fit_mlogit = mlogit(choice~feat+price, Data_mlogit, 
                    rpar=c(feat='n', price='n'),
                    R=100, halton=NA, print.level=0, panel=TRUE)
colnames(fit_mlogit$probabilities) = choices
multiclass.roc(ytest,fit_mlogit$probabilities)

ytrain = as.numeric(Yogurt$choice)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(choice ~ ., data = Yogurt)
probtest = fit_multinom$fitted.values
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

##### Tuna #####
tuna = Ecdat::Tuna
# Fit BART
ytrain = as.numeric(tuna$Tuna.choice)
xtrain = tuna[,-c(1,2,3)]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)
# Fit mlogit
Data_mlogit = mlogit.data(tuna, shape="wide", choice="Tuna.choice", varying=c(4:8))
fit_mlogit = mlogit(Tuna.choice~price, Data_mlogit)
ytest = tuna$Tuna.choice
multiclass.roc(ytest,fit_mlogit$probabilities)

ytrain = as.numeric(tuna$Tuna.choice)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(Tuna.choice ~ ., data = Tuna)
probtest = fit_multinom$fitted.values
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

# Apply monBART
# Adjust covariates so that relation with choice is monotone
tuna1 = xtrain
names(tuna1) = c("own","other1","other2","other3","other4")
for (i in 1:nrow(tuna1)){
  prices = xtrain[i,]
  choice = tuna$Tuna.choice[i]
  choices = levels(tuna$Tuna.choice)
  tuna1[i,1] = -prices[which(choice == choices)]
  tuna1[i,2:5] = prices[-which(choice == choices)]
}

fit_mbart = multi_monbart(x.train = tuna1, y.train = ytrain, x.test = tuna1, ndpost = n_mcmc)
#test.prob = apply(fit_mbart$prob.test,MARGIN=2,FUN=mean)
test.prob = fit_mbart$prob.test.mean
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

##### Car #####
data(Car)
# Fit BART
ytrain = as.numeric(Car$choice)
xtrain = Car[,-1]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)
# Fit mlogit
Car1 = Car
Car1$choice = factor(as.numeric(Car$choice))
for (i in 1:length(choices)){
  z = 4+i
  colnames(Car1)[z] = paste("type.",i)
}
for (i in 1:length(choices)){
  z = 10+i
  colnames(Car1)[z] = paste("fuel.",i)
}
for (i in 1:length(choices)){
  z = 16+i
  colnames(Car1)[z] = paste("price.",i)
}
for (i in 1:length(choices)){
  z = 22+i
  colnames(Car1)[z] = paste("range.",i)
}
for (i in 1:length(choices)){
  z = 28+i
  colnames(Car1)[z] = paste("acc.",i)
}
for (i in 1:length(choices)){
  z = 34+i
  colnames(Car1)[z] = paste("speed.",i)
}
for (i in 1:length(choices)){
  z = 40+i
  colnames(Car1)[z] = paste("pollution.",i)
}
for (i in 1:length(choices)){
  z = 46+i
  colnames(Car1)[z] = paste("size.",i)
}
for (i in 1:length(choices)){
  z = 52+i
  colnames(Car1)[z] = paste("space.",i)
}
for (i in 1:length(choices)){
  z = 58+i
  colnames(Car1)[z] = paste("cost.",i)
}
for (i in 1:length(choices)){
  z = 64+i
  colnames(Car1)[z] = paste("station.",i)
}
Data_mlogit = mlogit.data(Car1, shape="wide", choice="choice", varying=c(5:70), )
fit_mlogit = mlogit(choice~type+fuel+price+range+acc+speed+pollution
                    +size+space+cost+station, Data_mlogit)
colnames(fit_mlogit$probabilities) = choices
multiclass.roc(ytest,fit_mlogit$probabilities)

ytrain = as.numeric(Car$choice)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(choice ~ ., data = Car)
probtest = fit_multinom$fitted.values
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

##### ModeChoice #####
ytrain = ModeChoice$mode
choices = levels(ytrain)
ytest = as.numeric(ytrain)
# Fit mlogit
fit_mlogit = multinom(mode~ttme+invc+invt+gc+hinc+psize, data = ModeChoice)
library(cvAUC)
auc(ytest,fit_mlogit$fitted.values)

# Fit monotone BART
library(mBART)
# Change signs of covariates so that
# Every relationship is monotone increasing with Prob(Y=1) (choose car over train)
ytrain = ModeChoice$mode
xtrain = ModeChoice[,-1]
xtrain$invc = -xtrain$invc
xtrain$invt = -xtrain$invt
xtrain$psize = -xtrain$psize
xtrain = xtrain[,-6]

fit_monbart = mBART::monbart(x.train = xtrain, y.train = ytrain, ndpost = n_mcmc)
test.prob = apply(pnorm(fit_monbart$yhat.train),MARGIN=2,FUN=mean)
library(cvAUC)
ytest = ytrain
auc = AUC(test.prob,ytest)


##### Cracker #####
data(Cracker)
# Fit BART
ytrain = as.numeric(Cracker$choice)
xtrain = Cracker[,-14]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)
# Fit mlogit
Data_mlogit = mlogit.data(Cracker, shape="wide", id = "id", choice="choice", varying=c(2:13))
fit_mlogit = mlogit(choice~disp+price+feat, Data_mlogit, 
                    rpar=c(disp='n', price='n', feat='n'),
                    R=100, halton=NA, print.level=0, panel=TRUE)
colnames(fit_mlogit$probabilities) = choices
multiclass.roc(ytest,fit_mlogit$probabilities)

ytrain = as.numeric(Cracker$choice)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(choice ~ ., data = Cracker)
probtest = fit_multinom$fitted.values
colnames(probtest) = choices
multiclass.roc(ytest, probtest)
##### Fishing #####
data(Fishing)
# Fit BART
ytrain = as.numeric(Fishing$mode)
xtrain = Fishing[,-1]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)
# Fit mlogit
Fishing1 = Fishing
colnames(Fishing1) = c("mode","price","catch","p.beach","p.pier","p.boat",
                       "p.charter","c.beach","c.pier","c.boat","c.charter","income" )
Data_mlogit = mlogit.data(Fishing1, shape="wide", choice="mode", varying=c(4:11))
fit_mlogit = mlogit(mode~p+c|income, Data_mlogit)
colnames(fit_mlogit$probabilities) = choices
multiclass.roc(ytest,fit_mlogit$probabilities)

ytrain = as.numeric(Fishing$mode)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(mode ~ ., data = Fishing)
probtest = fit_multinom$fitted.values
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

##### Heating #####
data(Heating)
# Fit BART
ytrain = as.numeric(Heating$depvar)
xtrain = Heating[,-c(1,2)]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)
# Fit mlogit
Data_mlogit = mlogit.data(Heating, shape="wide", choice="depvar", varying=c(3:12,17:21))
fit_mlogit = mlogit(depvar~ic+oc+pb|income+agehed+rooms+region, Data_mlogit)
colnames(fit_mlogit$probabilities) = choices
multiclass.roc(ytest,fit_mlogit$probabilities)

ytrain = as.numeric(Heating$depvar)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(depvar ~ ., data = Heating)
probtest = fit_multinom$fitted.values
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

##### Ketchup #####
data(Ketchup)
# Fit BART
ytrain = as.numeric(Ketchup$Ketchup.choice)
xtrain = Ketchup[,-3]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)
# Fit mlogit
Data_mlogit = mlogit.data(Ketchup, shape="wide", id="Ketchup.hid", choice="Ketchup.choice",sep="." ,varying=c(4:7))
fit_mlogit = mlogit(Ketchup.choice~price, Data_mlogit, 
                    rpar=c(price='n'), R=100, halton=NA, print.level=0, panel=TRUE)
colnames(fit_mlogit$probabilities) = choices
multiclass.roc(ytest,fit_mlogit$probabilities)

ytrain = as.numeric(Ketchup$Ketchup.choice)
ytest = as.factor(ytrain)
choices = levels(ytest)
fit_multinom = multinom(Ketchup.choice ~ ., data = Ketchup)
probtest = fit_multinom$fitted.values
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

##### FriendFoe #####
data(FriendFoe)

# Fit BART
ytrain = as.numeric(FriendFoe$play)
xtrain = FriendFoe[,-4]
fit_bart = mbart(x.train = xtrain, y.train = ytrain, x.test = xtrain, ndpost = n_mcmc)
test.prob = apply(fit_bart$prob.test,MARGIN=2,FUN=mean)
ytest = as.factor(ytrain)
choices = levels(ytest)
probtest = t(matrix(test.prob, ncol = length(ytest)))
colnames(probtest) = choices
multiclass.roc(ytest, probtest)

# Fit Mixed GLM
fit_multinom = multinom(play ~ sex + white + age + season +
                          cash + sex1 + white1 + age1 + play1 + 
                          win + win1, data = FriendFoe)


#######################################
# Cross Validation
library(BART)
agg = Yogurt
agg$choice = as.numeric(agg.choice)
meancv = c()
for (it in 1:1){
library(caret)
# Create cross validation partitions: k-fold
K = 10
find = createFolds(y=as.factor(agg$yr5_mort), k = K, list = FALSE, returnTrain = FALSE)
cvauc = c()
for (k in 1:K)
{
tempind=which(find==k)
tr = agg[-tempind,]
te = agg[tempind,]

xtrain = tr[,c(-1,-10)]
ytrain = tr$choice
    
xtest = te[,c(-1,-10)]
ytest = te$choice
    
fit_bart.cv = mbart(x.train = xtrain, y.train = ytrain, x.test = xtest)
###getting prediction
test.prob = apply(pnorm(fit_bart.cv$yhat.test),MARGIN=2,FUN=mean)
pte = as.numeric(test.prob)
cte = ytest
library(pROC)
cvauc[k] = AUC(pte,cte)
}
meancv[it] = mean(cvauc)
}
mean(meancv)
