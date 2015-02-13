##################################################################
# Numerical Method to calculate Mean and Variance
# Use Recursion to accomodate new data
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################
set.seed(04252014); rm(list=ls())


#### Recursively Computing Mean
recursive.mean<-function(x,k){
vec1<-x; vec2<-c(0)
	for(i in 1:length(x)){
	vec2[i+1] <- vec2[i] + (vec1[i]-vec2[i])/i
	if(length(vec2)==k+1) break
	}
return(vec2[length(vec2)])
}

## Test
norm1<-rnorm(100)
mean(norm1)
recursive.mean(norm1,100)

#### Recursively computing Variance
recursive.variance<-function(x,k){
vec1<-x; vec2<-c(0)
for(i in 1:length(x)){
	vec2[i+1]<-(1-1/i)*vec2[i]+(i+1)*((recursive.mean(vec1,i+1)-recursive.mean(vec1,i))^2)
	if(length(vec2)==k+1) break
	}
return(vec2[length(vec2)-1])
}

## Test
norm2<-rnorm(100)
var(norm2)
recursive.variance(norm2,100)
