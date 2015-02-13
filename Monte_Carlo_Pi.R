##################################################################
# Estimation of Pi using Monte Carlo Simulation
# Randomly scatter points in the square and count how many
# point fall inside the circle
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################
set.seed(03282014); rm(list=ls())


## Step1: The Settings
library(shape)
plot(-1:1,ylim=c(-1.2,1.2),xlim=c(-1.2,1.2),type="n")
filledrectangle(mid=c(0,0),wx=2,wy=2,col=NULL,lcol="black",lwd=2)
plotellipse(rx=1,ry=1,mid=c(0,0))

## Step2: Creating (x,y)'s
U1<-runif(1000); U2<-runif(1000)
X<-2*U1-1; Y<-2*U2-1

## Step3: Ploting and Counting the dots
vec<-X^2+Y^2
points(X,Y,pch=16,cex=0.4)
points(X[which(vec<=1)],Y[which(vec<=1)],pch=17,cex=0.6,col="blue")

## Step4: Estimation of Pie
Estimated.Pie<-(length(vec[vec<=1])*4)/1000
cat("The Estimation of Pie is:",Estimated.Pie,"\n")
