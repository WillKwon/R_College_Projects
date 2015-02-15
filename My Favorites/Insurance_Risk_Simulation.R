##################################################################
# Insurance Risk Simulation
#
# The following code simulates whether an Insurance Company is
# still operating (not bankrupt) at the Endtime.
# The bank opens with initial Capital and Customers, and encounters
# three types of events: claim, new customer, losing customer
# in each instances, each variables will be updated.
# The company earns insurance premium after each accounting year
# At any evaluated moment, the company is considered as bankrupt if 
# if its Capital is negative 
#
# To have a meaningful insight, one needs to input meaningful parameters
#
# Will Daewook Kwon - will.dw.kwon@gmail.com
##################################################################


NewEvent = function(){
	rate = Customers*lambda + nu + Customers*mu
	x = rexp(1, rate)
	return (x)	
}

ClaimAmount = function(claim){
	x = rexp(1, claim)
	return (x)	
}

WhichEvent = function(){
	rate = Customers*lambda + nu + Customers*mu
	x = sample(c("claim","newCustomer","lostCustomer"), 1, replace = T, c( (Customers*lambda)/rate, nu/rate, (Customers*mu)/rate ) )
	return(x)
}

Simulator = function(Customers, Capital, Time, EndTime, lambda, nu, mu, claim, premium){
	repeat{
		X = NewEvent()
		eventTime = Time + X
		event = WhichEvent()
		
		if(event == "claim"){
			amount = ClaimAmount(claim)
			Capital = Capital - amount
		}
		else if(event == "newCustomer")
			Customers = Customers + 1
		else(event == "lostCustomer")
		Customers = Customers - 1
		
		if (Capital < 0)
			return(0)
		else{
			if(eventTime > EndTime)
				return(1)
			else{
				Capital = Capital + Customers*premium*(eventTime-Time)
				Time = eventTime
			}	
		}
	}
}

Survival_Rate = function(n, Customers, Capital, Time, EndTime, lambda, nu, mu, claim, premium){
	
	i = 0; count = 0
	while (i < n){
		x = Simulator(Customers, Capital, Time, EndTime, lambda, nu, mu, claim, premium)
		count = count + x
		i = i + 1
	}
	probability = count / n	
	return(probability)
	
}
	

## The Settings:
# Claims ~ pois(0.07) and Each Claim Amount ~ Exp(0.01)
# New Customers ~ pois(50)
# Time for policy holding ~ exp(0.05)
# Insurance Premium: 15 

## The Initial Conditions:
# Number of customers: 10,000
# Total Capital: 1,000,000 dollars
# Time = 0

Customers = 10000; Capital = 1000000; Time = 0
lambda = 0.07; nu = 50; mu = 0.05; claim = 0.05; premium = 15

## Demonstration
n = 10; EndTime = 3
Survival_Rate (n, Customers, Capital, Time, EndTime, lambda, nu, mu, claim, premium)









