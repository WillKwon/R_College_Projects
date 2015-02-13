##################################################################
# Generating Normal RV using Polar Method
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################
set.seed(20140411);rm(list=ls())


#### A Function for Generating R^2 values from U1 and U2
Rsquared<-function(n){
result<-list()
	for(i in 1:1e+8){
	U1<-runif(1);U2<-runif(1)
	V1<-2*U1-1;V2<-2*U2-1
	R<-V1^2+V2^2
	if(R<=1) {result[[i]]<-c(R,V1,V2)}
		else {result[[i]]<-NA}
	if(length(result[!is.na(result)])==n) {break}
}
result[!is.na(result)]
}

#### A Function for Generating Normal r.v's 
polarnormal<-function(n){
C<-Rsquared(ceiling(n/2));matrix<-matrix(0,2,ceiling(n/2))
	for(i in 1:ceiling(n/2)){
	x<-sqrt(-2*log(C[[i]][1]))*(C[[i]][2]/sqrt(C[[i]][1]))
	y<-sqrt(-2*log(C[[i]][1]))*(C[[i]][3]/sqrt(C[[i]][1]))
	matrix[,i]<-c(x,y)
	}
polarnormal<-as.vector(matrix)[1:n]
return(polarnormal)
}

#### Testing the Result
test1<-polarnormal(50);test1
test2<-polarnormal(1000)
hist(test2,freq=F); curve(dnorm,add=T)