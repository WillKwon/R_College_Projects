##################################################################
# Generation of Normal Random RVs using Rejection Method
# First Exponential RVs were generated using inverse CDF method
# Then Rejection Method was used
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################
set.seed(03292014); rm(list=ls())


#### Exponential RV using inverse CDF 
ExponentialRVGenerator<-function(n,lambda=1){
vec<-c()
	for(i in 1:n){
	vec[i]<-(-1/lambda)*log(runif(1))
	}
return(vec)
}

### Test ExponentialRVGenerator
test<-ExponentialRVGenerator(1000)
mean(test);var(test)

#### Normal RV using rejection method 
NormalRVGenerator<-function(n,mu=0,sigma=1){
vec<-c()
	for(i in 1:n){
		repeat{
		y1<-ExponentialRVGenerator(1,1)
		y2<-ExponentialRVGenerator(1,1)
			if(y2>=((y1-1)^2)/2) {break}
		}
	
	u<-runif(1)
	if(u<=1/2) {z=y1} else {z=-y1}
	vec[i]<-sigma*z+mu
	}
return(vec)
}

### Test NormalRVGenerator
normalrv<-NormalRVGenerator(1000)

mean(normalrv); sd(normalrv)
hist(normalrv,freq=F)
curve(dnorm,add=T,lwd=2.5)
