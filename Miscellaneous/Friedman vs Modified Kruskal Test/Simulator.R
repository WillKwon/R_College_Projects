#### The Simulator / Point Generator

pointgenerator = function(h){
	
	## Set up a frame
	countFR = 0; countKR=0
	windows(4.4,7); oopt = ani.options(int = 2, nmax = 100)
	
	## Set parameter & block & empty frame
	for(k in 1:500){ 
		muvec = c(3, 4+h, 5+2*h); sigmavec = c(0.5, 1, 1.5); vec = matrix(rep(0,3), ncol = 3)
		
		for(i in 1:3){
			for(j in 1:3){
				mu = muvec[i]; sigma = sigmavec[j]
				theta = rnorm(1, mu, sigma)
				data = c(theta, mu, sigma)
				vec = rbind(vec, data)
			}
		}
		
		DATA = vec[2:nrow(vec),]
		dimnames(DATA) <- list( 1:nrow(DATA), c("data", "group", "block")); DATA
		
		## Visualize data distribution
		table = data.frame("group1" = c(DATA[,1][DATA[,2] == 3]),
				"group2" = DATA[,1][DATA[,2] == 4+h], "group3" = DATA[,1][DATA[,2] == 5+2*h])
		plot(1:2, 1:2, xlim = c(0,4), ylim = c(0,12), xaxt = 'n', type = "n")
		boxplot(table,border = c("red", "black", "blue"), lwd = 3, add = T)
		
		## Trim data so it could be used in FRtest and KRtest
		fDATA=as.data.frame(DATA)
		b1mean = mean(DATA[,1][which(DATA[,3] == 0.5)])
		b2mean = mean(DATA[,1][which(DATA[,3] == 1)])
		b3mean = mean(DATA[,1][which(DATA[,3] == 1.5)])
		DATA[,1][which(DATA[,3] == 0.5)] = DATA[,1][which(DATA[,3] == 0.5)] - b1mean
		DATA[,1][which(DATA[,3] == 1)] = DATA[,1][which(DATA[,3] == 1)] - b2mean
		DATA[,1][which(DATA[,3] == 1.5)] = DATA[,1][which(DATA[,3] == 1.5)] - b3mean
		kDATA = DATA
		
		## Conduct tests & count # of rejection
		## friedman.test
		frtest = friedman.test(data ~ group|block,data = fDATA)
		## kruskal.test
		krtest = kruskal.test(data ~ group,data=kDATA)  
		
		if(frtest$p.value<=0.05) {countFR=countFR+1}
		if(krtest$p.value<=0.05) {countKR=countKR+1}
	}
	
	## Mark a point of the power function 
	return(c(countFR,countKR))
}
