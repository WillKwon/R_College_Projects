############################################################################################
# Project: Path Optimization using Simulated Annealing
# Will Daewook Kwon - will.dw.kwon@gmail.com
#
# Description:
# 10 Cities and their distance to each other are given. Using the Metropolis-Hastings
# algorith, we will generate more often those path with shorter distance. As the number
# of iteration grows, the generated path will converge to the shortest path possible.  
############################################################################################
rm(list=ls())


## The Distance Matrix
A<-c(0,587,1212,701,1936,604,748,2139,2182,543)
B<-c(0,0,920,940,1745,1188,713,1858,1737,597)
C<-c(0,0,0,879,831,1726,1631,949,1021,1494)
D<-c(0,0,0,0,1374,968,1420,1645,1891,1220)
E<-c(0,0,0,0,0,2339,2451,347,959,2300)
F<-c(0,0,0,0,0,0,1092,2594,2734,923)
G<-c(0,0,0,0,0,0,0,2571,2408,205)
H<-c(0,0,0,0,0,0,0,0,678,2442)
I<-c(0,0,0,0,0,0,0,0,0,2329)
J<-c(0,0,0,0,0,0,0,0,0,0)

distance<-cbind(A,B,C,D,E,F,G,H,I,J)
rownames(distance)=c("A","B","C","D","E","F","G","H","I","J")
distance=distance+t(distance)

## Distance Calculator: V(x)
DistCal<-function(vector){
vec<-c()
	for(i in 1:9){
		location<-vector[c(i,i+1)]
		vec[i]<-distance[location[1],location[2]]
	}
score=sum(vec)
return(score)
}

## y Generator: proposal function
## The number of neighbours are the same with 45=choose(10,2))
## Pick two arbitary indices and switch
Proposal<-function(vector){
co1<-sample(1:10,2)
co2=rev(co1)
vector[co1]<-vector[co2];y=vector
return(y)
}

## Set x0 (By letting A=1,B=2....K=10)
x0=c(1,2,3,4,5,6,7,8,9,10)

## Simulate the Markov Chain
Simulator<-function(N,x0){
collect<-list(x0)
x=x0
	for(i in 1:N){
	lambda<-1*log(1+i)
	y=Proposal(x)
	accept=min(1, exp(lambda*(-DistCal(y)/1000))/exp(lambda*(-DistCal(x)/1000)))
		if(runif(1)<=accept)
		{x=y} else {x=x}
	collect[[i]]<-x
	}
return(collect)
}

## Simulation
sim<-Simulator(10000,x0)

## Calculate distances of each vector in MC and ovserve distance reduction
vec<-c()
for(i in 1:10000){
z<-sim[[i]]
vec[i]<-DistCal(z)     
}

plot(ts(vec))

## The Limiting Distribution and the Maximum Value (Shortest Path)
optimal<-sim[length(sim)];optimal
DistCal(optimal[[1]])
