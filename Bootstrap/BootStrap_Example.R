##################################################################
# Estimation of Pi using Monte Carlo Simulation
# Randomly scatter points in the square and count how many
# point fall inside the circle
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################
rm(list=ls()); set.seed(04052014)


#### The Original Data
samp<-rnorm(30)
sampmean<-mean(samp);sampmedian<-median(samp)

#### The Bootstrap Sample
Bootstrapsamp<-list()
for(i in 1:100){
	Bootstrapsamp[[i]]<-sample(samp,30,replace=T)
}

#### Bootstrap mean, median, and MSE
bootmean<-sapply(Bootstrapsamp,mean)
bootmedian<-sapply(Bootstrapsamp,median)
MSEmean<-sum((bootmean-sampmean)^2)/100
MSEmedian<-sum((bootmedian-sampmedian)^2)/100

#### Show the Result
par(mfcol=c(1,2))
hist(bootmean,prob=T);hist(bootmedian,prob=T)
summary(bootmean);summary(bootmedian)
sampmean;sampmedian
cat(" Theoretical MSEs of sample mean and median from the standard normal distribution are:","\n",
		"Mean:", 1/30," Median:", (1.253/sqrt(30))^2, "\n",
		"Computed MSEs of bootstrap sample mean and median are:","\n",
		"Mean:", MSEmean," Median:", MSEmedian, "\n")

