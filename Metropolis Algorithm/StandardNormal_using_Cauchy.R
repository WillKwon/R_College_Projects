##################################################################
# The following code simulates Standard Normal RV's using the 
# method of Metropolis-Hastings Algorithm
# The Cauchy distr was used as the proposal distr
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################
set.seed(190021); rm(list=ls())


# Target Distribution: Standard Normal
# Proposal Distribution: Cauchy Distribution 
Cauchy = function(x) dcauchy(x,0,1)
Normal = function(x) dnorm(x,0,1)

curve(Normal, xlim = c(-4,4),)
curve(Cauchy, xlim = c(-4,4),add=T)
# Cauchy is quite close to the Standard Normal
# Hence, it is a good choice as a proposal

# Now Construct our function
MCMC_Normal = function(n, initial = 0){
	current = initial
	MC = c(); i = 0
	# 2000 denotes burn-in
	length = n + 2000
	
	while( i <= length){
		# Proposal
		nex = rcauchy(1, current, 1)
		
		# Caculate Acceptance
		U = runif(1)
		accept = (dnorm(nex)*dcauchy(nex, current, 1)) / (dnorm(current)*dcauchy(current, nex, 1)) 
		if( U <= accept )
			current = nex	
		else
			current = current
		
		# Collect
		MC[i] = current
		i = i + 1
	}
	
	# Discard the Burn-in
	return( MC[2001:length] )
}

# Demonstration
SimulatedNormal = MCMC_Normal(10000)
length(SimulatedNormal)
hist(SimulatedNormal, prob = T, xlim = c(-4,4))
curve(dnorm, add = T, col = "red", xlim = c(-4,4))
