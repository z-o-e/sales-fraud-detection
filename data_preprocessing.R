###################################################
#
#	Sales fraud detection-Data preprocessing
#
###################################################

# install.packages('DMwR')
library(DMwR)


# load data
load('sales.Rdata')
data(sales)



###################################################
# exploring the data set
###################################################

head(sales)
summary(sales)
nlevels(sales$Prod)


# install.packages('Hmisc')

library('Hmisc')

# a lot of missing values in Quant and Val

describe(sales)


# missing pairs of Quant & Val ---888

# lentgh(which(is.na(sales$Quant) & is.na(sales$Val)))
sum(is.na(sales$Quant) & is.na(sales$Val))


# inspection proportion

table(sales$Insp)/nrow(sales)*100


# variability in figures

totS<-table(sales$ID)

totP<-table(sales$Prod)

barplot(totS,main="Transactions per salespeople", 
	names.arg="",xlab='Salespeople',ylab='Amount')
	
barplot(totP,main='Transactions per product',
	names.arg='',xlab='Products',ylab='Amount')


# try to solve variability by introducing a new column sales per unit

sales$Uprice<-sales$Val/sales$Quant

summary(sales$Uprice)


# look into top and bottom price tiers

attach(sales)

upp <- aggregate(Uprice,list(Prod),median,na.rm=T)

topP <- sapply(c(T,F),function(o) 
               upp[order(upp[,2],decreasing=o)[1:5],1])

colnames(topP) <- c('Expensive','Cheap')
topP

tops <- sales[Prod %in% topP[1,],c('Prod','Uprice')]

# convert to factor so tops$Prob don't have same levels as original set

tops$Prod <- factor(tops$Prod)

# scale by log

boxplot(Uprice ~ Prod,data=tops,ylab='Uprice',log="y")



# look into top and bottom sales units tiers (similar approach to price)

vs <- aggregate(Val,list(ID),sum,na.rm=T)

scoresSs <- sapply(c(T,F),function(o) 
                   vs[order(vs$x,decreasing=o)[1:5],1])

colnames(scoresSs) <- c('Most','Least')
scoresSs

# top 100 sales people generate about 40% of the total income

sum(vs[order(vs$x,decreasing=T)[1:100],2])/sum(Val,na.rm=T)*100

# bottom 20% sales people generate 2% of the income

sum(vs[order(vs$x,decreasing=F)[1:2000],2])/sum(Val,na.rm=T)*100



# look into sales quantity sold for each product

qs <- aggregate(Quant,list(Prod),sum,na.rm=T)

scoresPs <- sapply(c(T,F),function(o) 
                   qs[order(qs$x,decreasing=o)[1:5],1])

colnames(scoresPs) <- c('Most','Least')

scoresPs

# top 100 products represent about 75% of the sales

sum(as.double(qs[order(qs$x,decreasing=T)[1:100],2]))/
  sum(as.double(Quant),na.rm=T)*100

# bottom 4000 represent about 9% of the sales

sum(as.double(qs[order(qs$x,decreasing=F)[1:4000],2]))/
  sum(as.double(Quant),na.rm=T)*100


# determine number of outliers

out <- tapply(Uprice,list(Prod=Prod),
              function(x) length(boxplot.stats(x)$out))

# top ten product in terms of number of outliers
out[order(out,decreasing=T)[1:10]]

# number of outliers
sum(out)

# proportion of outliers in total number of transactions
sum(out)/nrow(sales)*100



###################################################
#	data problems (missing values and sparcity)
###################################################

# total number of transactions per salesperson

totS <- table(ID)

# total number of transactions per product

totP <- table(Prod)

# cases with na Quant and na Val

nas <- sales[which(is.na(Quant) & is.na(Val)),c('ID','Prod')]

# salespeople IDs with missing Quant & Val

propS <- 100*table(nas$ID)/totS

# top ten salespeople with the most missing data
# all below 15%, safe to remove 

propS[order(propS,decreasing=T)[1:10]]

# products with missing with missing Quant & Val

propP <- 100*table(nas$Prod)/totP

# top ten products with the most missing data
# some product have about 40% of missing values
# can join with similar products

propP[order(propP,decreasing=T)[1:10]]

# detatch and delete cases with na Quant & na Val

detach(sales)
sales <- sales[-which(is.na(sales$Quant) & is.na(sales$Val)),]


# remaining products with either na Quant or na Val

nnasQp <- tapply(sales$Quant,list(sales$Prod),
                 function(x) sum(is.na(x)))

propNAsQp <- nnasQp/table(sales$Prod)

# product p2442 & p2443 with all na Quant

propNAsQp[order(propNAsQp,decreasing=T)[1:10]]

# delete p2442 & p2443
 
sales <- sales[!sales$Prod %in% c('p2442','p2443'),]


# remaining salespersons with either na Quant or na Val

nnasQs <- tapply(sales$Quant,list(sales$ID),function(x) sum(is.na(x)))

propNAsQs <- nnasQs/table(sales$ID)

propNAsQs[order(propNAsQs,decreasing=T)[1:10]]


# products with unknown values in the Val column

nnasVp <- tapply(sales$Val,list(sales$Prod),function(x) sum(is.na(x)))

propNAsVp <- nnasVp/table(sales$Prod)

# missing proportion is reasonable

propNAsVp[order(propNAsVp,decreasing=T)[1:10]]


# salespersons with unknow values in the Val column

nnasVs <- tapply(sales$Val,list(sales$ID),function(x) sum(is.na(x)))

propNAsVs <- nnasVs/table(sales$ID)

# proportion is reasonable

propNAsVs[order(propNAsVs,decreasing=T)[1:10]]


# now have now transaction with both Quant & Val missing
# replace all remaining missing values by using unit prices
 
tPrice <- tapply(sales[sales$Insp != 'fraud','Uprice'],
				  list(sales[sales$Insp !='fraud','Prod']),median,na.rm=T)


noQuant <- which(is.na(sales$Quant))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] /
                                  tPrice[sales[noQuant,'Prod']])

noVal <- which(is.na(sales$Val))
sales[noVal,'Val'] <- sales[noVal,'Quant'] *
                      tPrice[sales[noVal,'Prod']]


# recalculate unit price

sales$Uprice <- sales$Val/sales$Quant

save(sales,file='salesClean.Rdata')

attach(sales)



notF <- which(Insp != 'fraud')

# find out most similar products by using mean and IQR

ms <- tapply(Uprice[notF],list(Prod=Prod[notF]),function(x) {
     bp <- boxplot.stats(x)$stats
     c(median=bp[3],iqr=bp[4]-bp[2])
   })
ms <- matrix(unlist(ms),
             length(ms),2,
             byrow=T,dimnames=list(names(ms),c('median','iqr')))
head(ms)


par(mfrow=c(1,2))
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='')
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='',col='grey',log="xy")
smalls <- which(table(Prod) < 20)
points(log(ms[smalls,1]),log(ms[smalls,2]),pch='+')


dms <- scale(ms)
smalls <- which(table(Prod) < 20)
prods <- tapply(sales$Uprice,sales$Prod,list)
similar <- matrix(NA,length(smalls),7,dimnames=list(names(smalls),c('Simil','ks.stat','ks.p','medP','iqrP','medS','iqrS')))

for(i in seq(along=smalls)) {
  d <- scale(dms,dms[smalls[i],],FALSE)
  d <- sqrt(drop(d^2 %*% rep(1,ncol(d))))
  stat <- ks.test(prods[[smalls[i]]],prods[[order(d)[2]]])
  similar[i,] <- c(order(d)[2],stat$statistic,stat$p.value,ms[smalls[i],],ms[order(d)[2],])
}


head(similar)


levels(Prod)[similar[1,1]]


nrow(similar[similar[,'ks.p'] >= 0.9,])


sum(similar[,'ks.p'] >= 0.9)


save(similar,file='similarProducts.Rdata')


