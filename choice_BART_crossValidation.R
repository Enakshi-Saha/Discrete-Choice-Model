source('~/Desktop/Discrete Choice/Code/mon.bart.R')
source('~/Desktop/Discrete Choice/Code/multi_monbart.R')

set.seed(21)

library(BART)          # BART Model
library(bayesm)        # Hierarchical Bayesian Model
library(mlogit)       # Mixed Effects GLM
library(ISLR)          # Orange Juice Data
library(Ecdat)         # Multiple econometrics data
library(pROC)          # Multiclass ROC curve

n_mcmc = 1000        # Number of posterior samples for BART
n_cv = 4 # Number of CV partition
iter = 5 # Number of Random partitions

# Cross Validation: Tuna
tuna = Ecdat::Tuna
ytrain = as.numeric(tuna$Tuna.choice)
xtrain = tuna[,-c(1,2,3)]
tuna1 = xtrain
names(tuna1) = c("own","other1","other2","other3","other4")
for (i in 1:nrow(tuna1)){
  prices = xtrain[i,]
  choice = tuna$Tuna.choice[i]
  choices = levels(tuna$Tuna.choice)
  tuna1[i,1] = -prices[which(choice == choices)]
  tuna1[i,2:5] = prices[-which(choice == choices)]
}
agg_mbart = cbind(ytrain,tuna1)
agg_bart = cbind(ytrain,xtrain)
meancv_bart = c()
meancv_mbart = c()
for (it in 1:iter){
  library(caret)
  # Create cross validation partitions: k-fold
  K = n_cv
  find = createFolds(agg_bart$ytrain, k = K, list = FALSE, returnTrain = FALSE)
  cvauc_bart = rep(0,K)
  cvauc_mbart = rep(0,K)
  for (k in 1:K)
  {
    tempind=which(find==k)
    tr_bart = agg_bart[-tempind,]
    te_bart = agg_bart[tempind,]
    tr_mbart = agg_mbart[-tempind,]
    te_mbart = agg_mbart[tempind,]
    
    xtrain_bart = tr_bart[,-1]
    ytrain_bart = tr_bart[,1]
    xtrain_mbart = tr_mbart[,-1]
    ytrain_mbart = tr_mbart[,1]
    
    xtest_bart = te_bart[,-1]
    ytest_bart = te_bart[,1]
    xtest_mbart = te_mbart[,-1]
    ytest_mbart = te_mbart[,1]
    
    fit_bart.cv = mbart(x.train = xtrain_bart, y.train = ytrain_bart, 
                        x.test = xtest_bart, ndpost = n_mcmc)
    fit_mbart.cv = multi_monbart(x.train = xtrain_mbart, y.train = ytrain_mbart,
                                 x.test = xtest_mbart, ndpost = n_mcmc)
    #fit_multinom.cv = multinom(ytrain~own+other1+other2+other3+other4,data=agg,subset=setdiff(1:nrow(agg),tempind))
    
    ###getting prediction
    test.prob_bart = fit_bart.cv$prob.test.mean
    test.prob_mbart = fit_mbart.cv$prob.test.mean
   
    choices_bart = levels(factor(ytest_bart))
    choices_mbart = levels(factor(ytest_mbart))
    
    probtest_bart = t(matrix(test.prob_bart, ncol = length(ytest_bart)))
    probtest_mbart = t(matrix(test.prob_mbart, ncol = length(ytest_mbart)))
    
    colnames(probtest_bart) = choices_bart
    colnames(probtest_mbart) = choices_mbart
    
    cvauc_bart[k] = multiclass.roc(ytest_bart, probtest_bart)$auc
    cvauc_mbart[k] = multiclass.roc(ytest_mbart, probtest_mbart)$auc
  }
  meancv_bart[it] = mean(cvauc_bart)
  meancv_mbart[it] = mean(cvauc_mbart)
}
auc_bart=mean(meancv_bart)
auc_mbart=mean(meancv_mbart)
auc_tuna = c(auc_bart,auc_mbart)

# Cross Validation: OJ
ytrain = OJ$Purchase
ytrain = as.numeric(ytrain)
xtrain = OJ[,-c(1,2,3,14,18)]
xtrain1 = xtrain
xtrain1$PriceMM = -xtrain$PriceMM
xtrain1$DiscCH = -xtrain$DiscCH
xtrain1$SpecialCH = -xtrain$SpecialCH
xtrain1$LoyalCH = -xtrain$LoyalCH
xtrain1$SalePriceMM = -xtrain$SalePriceMM
xtrain1$PriceDiff = -xtrain$PriceDiff
xtrain1$PctDiscCH = -xtrain$PctDiscCH
xtrain1$ListPriceDiff = -xtrain$ListPriceDiff

agg_mbart = cbind(ytrain,xtrain1)
agg_bart = cbind(ytrain,xtrain)
meancv_bart = c()
meancv_mbart = c()
for (it in 1:iter){
  library(caret)
  # Create cross validation partitions: k-fold
  K = n_cv
  find = createFolds(agg_bart$ytrain, k = K, list = FALSE, returnTrain = FALSE)
  cvauc_bart = rep(0,K)
  cvauc_mbart = rep(0,K)
  for (k in 1:K)
  {
    tempind=which(find==k)
    tr_bart = agg_bart[-tempind,]
    te_bart = agg_bart[tempind,]
    tr_mbart = agg_mbart[-tempind,]
    te_mbart = agg_mbart[tempind,]
    
    xtrain_bart = tr_bart[,-1]
    ytrain_bart = tr_bart[,1]
    xtrain_mbart = tr_mbart[,-1]
    ytrain_mbart = tr_mbart[,1]
    
    xtest_bart = te_bart[,-1]
    ytest_bart = te_bart[,1]
    xtest_mbart = te_mbart[,-1]
    ytest_mbart = te_mbart[,1]
    
    fit_bart.cv = mbart(x.train = xtrain_bart, y.train = ytrain_bart, 
                        x.test = xtest_bart, ndpost = n_mcmc)
    fit_mbart.cv = mBART::monbart(x.train = xtrain_mbart, y.train = ytrain_mbart,
                                 x.test = xtest_mbart, ndpost = n_mcmc)
    #fit_multinom.cv = multinom(ytrain~own+other1+other2+other3+other4,data=agg,subset=setdiff(1:nrow(agg),tempind))
    
    ###getting prediction
    test.prob_bart = fit_bart.cv$prob.test.mean
    test.prob_mbart = apply(pnorm(fit_mbart.cv$yhat.test),MARGIN=2,FUN=mean)
    
    choices_bart = levels(factor(ytest_bart))
    choices_mbart = levels(factor(ytest_mbart))
    
    probtest_bart = t(matrix(test.prob_bart, ncol = length(ytest_bart)))
    probtest_mbart = t(matrix(test.prob_mbart, ncol = length(ytest_mbart)))
    
    colnames(probtest_bart) = choices_bart
    #colnames(probtest_mbart) = choices_mbart
    
    cvauc_bart[k] = multiclass.roc(ytest_bart, probtest_bart)$auc
    library(cvAUC)
    cvauc_mbart[k] = auc(ytest_mbart, probtest_mbart)
  }
  meancv_bart[it] = mean(cvauc_bart)
  meancv_mbart[it] = mean(cvauc_mbart)
}
auc_bart=mean(meancv_bart)
auc_mbart=mean(meancv_mbart)
auc_oj = c(auc_bart,auc_mbart)



