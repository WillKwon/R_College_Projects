##################################################################
# Expected Payoff of the Non-standard Call Option with Barrier Condition
#
# The following code employed a slightly complicated version that 
# uses variance reduction technique. To reduce variance and attain
# convergence faster, it generates only the prices that have crossed
# the barrier condition.# 
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################
rm(list=ls()); set.seed(05092014)


## Call Option
InitialPrice = 100; StrikingPrice = 100; ExpirationDate = 20;   
# Barrier Condition
Barrier = 101; BarrierTime = 15
# Geometric Brownian Motion Parameters
Drift = 0.05; Volatility = 0.3;


## PROCEDURES
Compute_C = function(InitialPrice, Barrier, BarrierTime, Drift, Volatility){
	c = (log((Barrier/InitialPrice)) - BarrierTime*Drift) / (Volatility*sqrt(BarrierTime))
	return(c)
}

Compute_Lambda = function(c){
	lambda = ( c + sqrt(c^2 + 4)) / 2
	return(lambda)
}

Conditioned_X = function(c, lambda){
	repeat{
		U1 = runif(1)
		Y = (-1/lambda)*log(U1) + c    # Y ~ Expo(lambda) shifted by c
	
		U2 = runif(1)
		if(U2 <= exp((-(Y - lambda)^2) / 2)){
			X = BarrierTime*Drift + Volatility*sqrt(BarrierTime)*Y
			return(X)
		}
	}
}

Simulate_Option = function (n, InitialPrice, StrikingPrice, ExpirationDate, Barrier, BarrierTime, Drift, Volatility){	
	# Compute upper probability (adjustment for using conditioned X's
	Adjustment = pnorm( (log(Barrier/InitialPrice) - BarrierTime*Drift) / Volatility*sqrt(BarrierTime), lower.tail = F)
	
	# Compute necessary variables
	C = Compute_C(InitialPrice, Barrier, BarrierTime, Drift, Volatility) 
	Lambda = Compute_Lambda(C)
	
	# Expected Payoff
	i = 1; Record = c()
	while(i <= n){
		X = Conditioned_X(C, Lambda)
		Y = rnorm(1, (ExpirationDate - BarrierTime)*Drift, (ExpirationDate - BarrierTime)*Volatility^2)   		
		Payoff = InitialPrice*exp(X + Y) - StrikingPrice
		
		if (Payoff <= 0)
			Payoff = 0
		
		Record[i] = Payoff
		i = i + 1
	}
	
	ExpectedPayoff = mean(Record)*Adjustment
	return(ExpectedPayoff)
}


## Demonstration
Simulate_Option(1000, InitialPrice, StrikingPrice, ExpirationDate, Barrier, BarrierTime, Drift, Volatility)	




