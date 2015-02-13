##################################################################
# MCMC Examples
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################

# Example_1:
# Explain how to use a MCMC to generate the value of a rnadom vector 
# X1 ~ X10 whose distr is approximately the conditional distr of
# 10 independent exponential random variables with common mean 1
# given that prod(Xi)(i=1~10) > 20
##################################################################
rm(list=ls());set.seed(06092014)


## Initial Value
x0=c(a=1.5,b=1.5,c=1.5,d=1.5,e=1.5,f=1.5,g=1.5,h=1.5,i=1.5,j=1.5)
sample=as.matrix(x0)

repeat{
	## Proposal such that prod(x1~x10)>20
	proposal=0
	while(prod(proposal)<20){
		proposal<-rexp(10,1/1.5)
	}
	
	pronext=(1/1.5)^10*exp(-(1/1.5)*sum(proposal))
	propre=(1/1.5)^10*exp((-1/1.5)*sum(sample[,dim(sample)[2]]))
	pdfnext=exp(-sum(proposal))
	pdfpre=exp(-sum(sample[,dim(sample)[2]]))
	
	## Acceptance Ratio
	accpet=min(1,(pdfnext*propre)/(pdfpre*pronext))
	if(runif(1)<accpet){sample<-cbind(sample,proposal)}
	
	if(dim(sample)[2]==1000) {break}
}

X1=sample[1,];X2=sample[2,];par(mfcol=c(2,1))
hist(X1,10,freq=F);curve(dexp(x,1),add=T,col="red",lwd=2)
hist(X2,10,freq=F);curve(dexp(x,1),add=T,col="red",lwd=2)
par(mfcol=c(1,1));plot(X1,X2);cor(X1,X2)




##################################################################
# Example_2:
# Let Xi, i = 1,2,3 be independent exponentials with mean 1. 
# Run a simulation study to estimate
# E[X1+2X2+3X3 / X1+2X2+3X3 > 15]
##################################################################
rm(list=ls());set.seed(06092014)


## Set the initial Value
x0=c(X1=2,X2=2,X3=2); sample=as.matrix(x0)

repeat{
	proposal=c(0,0,0)
	while(proposal[1]+2*proposal[2]+3*proposal[3]<15){    ## Proposal such that Sum(X1+2X2+3X3)>15
		proposal<-rexp(3,1/2)
	}
	
	pronew=(1/2)^10*exp(-(1/2)*sum(proposal))    ## For calculating the acceptance ratio
	proold=(1/2)^10*exp((-1/2)*sum(sample[,dim(sample)[2]]))
	pdfnew=exp(-sum(proposal))
	pdfold=exp(-sum(sample[,dim(sample)[2]]))
	
	accpet=min(1,(pdfnew*proold)/(pdfold*pronew))    ## Acceptance Ratio
	if(runif(1)<accpet){sample<-cbind(sample,proposal)}
	
	if(dim(sample)[2]==1000) {break}    ## Collect 1000 samples
}

X1=sample[1,];X2=sample[2,];X3=sample[3,];par(mfcol=c(3,1))
hist(X1,10,freq=F);curve(dexp(x,1),add=T,col="red",lwd=2)
hist(X2,10,freq=F);curve(dexp(x,1),add=T,col="red",lwd=2)
hist(X3,10,freq=F);curve(dexp(x,1),add=T,col="red",lwd=2)

mean(X1+2*X2+3*X3)




##################################################################
# Example_3:
# Use the SIR algorithm to generate a permuation of 1, 2, .. 100
# whose distr is approximately that of a random permutation 
# X1 ~X100 conditioned on the event that sum(j*Xj) > 285,000
##################################################################
rm(list=ls()); set.seed(06092014)


start<-1:100; weight<-1:100; set<-list() 
## Generate 1000 sets of permutations such that the condition is satisfied
repeat{
	vector=0
	while(sum(vector*weight)<285000){
		vector<-sample(start,100,replace=F)
	}
	set[[length(set)+1]]<-vector
	if(length(set)==1000) {break}
}

## x is chosen with prob=1/length(set)
collect<-rep(0,1000)
for(i in 1:100){
	index<-sample(1:1000,1)
	collect<-rbind(collect,set[[index]])
}

## Extract X1 and X100
X1<-collect[-1,1]; X100<-collect[-1,100]
summary(X1);summary(X100);var(X1);var(X100)
par(mfcol=c(2,1))
hist(X1);hist(X100)
