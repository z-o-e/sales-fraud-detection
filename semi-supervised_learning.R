###################################################
#
#	Sales fraud detection- Semi-supervised Learning
#
###################################################

source("Prob_1_data_preprocessing.R")
source("Prob_1_data_supervised_learning.R")


###################################################
### Semi-supervised approaches
###################################################

set.seed(1200)
pred.gb <- function(m,d) {
  p <- predict(m,d,type='probability')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
             )
}

gb.st <- function(train,test) {
    
  train <- train[,c('ID','Prod','Uprice','Insp')]
  train[which(train$Insp == 'unkn'),'Insp']<-NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  train$Prod=as.numeric(train$Prod)
  train$ID=as.numeric(train$ID)
  model <- SelfTrain(Insp ~ .,train,learner('gbm',distribution = "bernoulli")),
                     'pred.gb')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probability')
  return(list(rankOrder=order(preds,decreasing=T),
              rankScore=preds)
         )
}

ho.gb.st <- function(form, train, test, ...) {
  res <- gb.st(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
                       )
           )
}

gb.st.res <- holdOut(learner('ho.gb.st',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
                  )


summary(gb.st.res)


info <- attr(gb.st.res,'itsInfo')

PTs.gb.st <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
                )

PRcurve(PTs.gb[,,1],PTs.gb[,,2],
        main='PR curve',lty=1,col='green',xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
        
PRcurve(PTs.gb.st[,,1],PTs.gb.st[,,2],
        add=T,lty=1,col='red2',
        avg='vertical')    
            
legend('topright',c('supervised gradient boost','semi-supervised gradient boost'),
       lty=c(1,1),col=c('green','red2'))

