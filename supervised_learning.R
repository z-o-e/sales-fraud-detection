###################################################
#
#	Sales fraud detection- Supervised Learning
#
###################################################

source("Prob_1_data_preprocessing.R")

###################################################
# Evaluation criteria (Precision & Recall)
###################################################

# precision & recall plot

PRcurve <- function(preds,trues,...) {
  require(ROCR,quietly=T)
  pd <- prediction(preds,trues)
  pf <- performance(pd,'prec','rec')
  pf@y.values <- lapply(pf@y.values,function(x) rev(cummax(rev(x))))
  plot(pf,...)
}

avgNDTP <- function(toInsp,train,stats) {
  if (missing(train) && missing(stats)) 
    stop('Provide either the training data or the product stats')
  if (missing(stats)) {
    notF <- which(train$Insp != 'fraud')
    stats <- tapply(train$Uprice[notF],
                    list(Prod=train$Prod[notF]),
                    function(x) {
                      bp <- boxplot.stats(x)$stats
                      c(median=bp[3],iqr=bp[4]-bp[2])
                    })
    stats <- matrix(unlist(stats),
                    length(stats),2,byrow=T,
                    dimnames=list(names(stats),c('median','iqr')))
    stats[which(stats[,'iqr']==0),'iqr'] <- 
        stats[which(stats[,'iqr']==0),'median']
  }

  mdtp <- mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median']) /
               stats[toInsp$Prod,'iqr'])
  return(mdtp)
}


prf <- function(predAct){
    ## predAct is two col dataframe of pred,act
    preds = predAct[,1]
    trues = predAct[,2]
    xTab <- table(preds, trues)
    clss <- as.character(sort(unique(preds)))
    r <- matrix(NA, ncol = 7, nrow = 1, 
        dimnames = list(c(),c('Accuracy',
        'MissPrecision','Miss Recall',
        'Hit Precision','Hit Recall',
        'Hit F')))
    r[1,1] <- sum(xTab[1,1],xTab[2,2])/sum(xTab) # Accuracy
    r[1,2] <- xTab[1,1]/sum(xTab[,1]) # Miss Precision
    r[1,3] <- xTab[1,1]/sum(xTab[1,]) # Miss Recall
    r[1,4] <- (2*r[1,2]*r[1,3])/sum(r[1,2],r[1,3]) # Miss F
    r[1,5] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
    r[1,6] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
    r[1,7] <- (2*r[1,5]*r[1,6])/sum(r[1,5],r[1,6]) # Hit F
    r}

###################################################
# Experimental Methodology
###################################################
evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds) {
  ordTS <- testSet[rankOrder,]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
  cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
  prec <- cm['fraud','fraud']/sum(cm['fraud',])
  rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
  AVGndtp <- avgNDTP(ordTS[nF,],stats=statsProds)
  return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}



notF <- which(sales$Insp != 'fraud')

globalStats <- tapply(sales$Uprice[notF],
                      list(Prod=sales$Prod[notF]),
                      function(x) {
                        bp <- boxplot.stats(x)$stats
                        c(median=bp[3],iqr=bp[4]-bp[2])
                      })
globalStats <- matrix(unlist(globalStats),
                length(globalStats),2,byrow=T,
                dimnames=list(names(globalStats),c('median','iqr')))
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- 
   globalStats[which(globalStats[,'iqr']==0),'median']


###################################################
# AdaBoost and Gradient Boost
###################################################

# adaBoost with adaBoostM1

library(RWeka)
WOW(AdaBoostM1)
ab.M1 <- function(train,test) {
  require(RWeka,quietly=T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  model <- AdaBoostM1(Insp ~ .,data,
                      control=Weka_control(I=100))
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probability')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
         )
}


ho.ab.M1 <- function(form, train, test, ...) {
  res <- ab.M1(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


ab.M1.res <- holdOut(learner('ho.ab.M1',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(ab.M1.res)



# ada boost with ada

# install.packages('ada')
library(ada)

ab <- function(train,test) {
  dc<-rpart.control()
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  model <- ada(Insp ~ .,data, control=dc,type="discrete")
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probs')
  return(list(rankOrder=order(preds[,1],decreasing=T),
              rankScore=preds[,1])
         )
}


ho.ab <- function(form, train, test, ...) {
  res <- ab(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


ab.res <- holdOut(learner('ho.ab',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )

summary(ab.res)



# gradient boost with gbm

# install.packages('gbm')
library(gbm)

gb <- function(train,test) {
  
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  data$Prod=as.numeric(data$Prod)
  data$ID=as.numeric(data$ID)
  data$Insp=as.numeric(data$Insp)-1
  model <- gbm(Insp ~ .,data,distribution = "bernoulli")
  test$Prod=as.numeric(test$Prod)
  test$ID=as.numeric(test$ID)
  test$Insp=as.numeric(test$Insp)-1
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],n.trees=100,type="response")
  return(list(rankOrder=order(preds,decreasing=T),
              rankScore=preds)
         )
}


ho.gb <- function(form, train, test, ...) {
  res <- gb(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


gb.res <- holdOut(learner('ho.gb',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )

summary(gb.res)

# plot PR curves

par(mfrow=c(1,1))

info <- attr(ab.res,'itsInfo')
PTs.ab <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2))

info <- attr(gb.res,'itsInfo')
PTs.gb <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2))
                
info <- attr(ab.M1.res,'itsInfo')
PTs.ab.M1 <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2))
            
PRcurve(PTs.ab[,,1],PTs.ab[,,2],
        main='PR curve',lty=1,col='red2',xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
       
PRcurve(PTs.gb[,,1],PTs.gb[,,2],
        add=T,lty=1,col='blue',
        avg='vertical')
        
PRcurve(PTs.ab.M1[,,1],PTs.ab.M1[,,2],
        add=T,lty=2,col='green',
        avg='vertical')        

legend('topright',c('AdaBoost','GradientBoost','AdaBoostM1'),
       lty=c(1,1,2), col=c('red2','blue','green'))



###################################################
# Two other supervised learning methods
###################################################

# logistic regression

lg <- function(train,test) {
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  data$Prod=as.numeric(data$Prod)
  data$ID=as.numeric(data$ID)
  model <- glm(Insp ~ .,data,family = binomial)
  test$Prod=as.numeric(test$Prod)
  test$ID=as.numeric(test$ID)
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='response')
  return(list(rankOrder=order(preds,decreasing=T),
              rankScore=preds)
         )
}

ho.lg <- function(form, train, test, ...) {
  res <- lg(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


lg.res <- holdOut(learner('ho.lg',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(lg.res)

info <- attr(lg.res,'itsInfo')
PTs.lg <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2))


# Mars

# install.packages('earth')

library(earth)
mars <- function(train,test) {
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  data$Prod=as.numeric(data$Prod)
  data$ID=as.numeric(data$ID)
  model <- earth(Insp ~ .,data,glm = list(family = binomial))
  test$Prod=as.numeric(test$Prod)
  test$ID=as.numeric(test$ID)
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='response')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
         )
}


ho.mars <- function(form, train, test, ...) {
  res <- mars(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}


mars.res <- holdOut(learner('ho.mars',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(mars.res)

info <- attr(mars.res,'itsInfo')
PTs.mars <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2))

# PR curves

PRcurve(PTs.ab[,,1],PTs.ab[,,2],
        main='PR curve',lty=1,col='red2',xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
       
PRcurve(PTs.gb[,,1],PTs.gb[,,2],
        add=T,lty=1,col='blue',
        avg='vertical')

PRcurve(PTs.mars[,,1],PTs.mars[,,2],
        add=T,lty=1,col='green',
        avg='vertical')


PRcurve(PTs.lg[,,1],PTs.lg[,,2],
        add=T,lty=1,col='orange',
        avg='vertical')

legend('topright',c('AdaBoost','GradientBoost','Mars','Logistic'),
       lty=c(1,1,1,1), col=c('red2','blue','green',"orange"))




