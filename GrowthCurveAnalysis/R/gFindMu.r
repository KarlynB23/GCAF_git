gFindMu <- function(gc.spline, dydt.spline, dydtdt.spline){
# Uses a relatively intelligent process based on biological properties of Halobacteria to choose maximum growth rate etc.
# Args:
#	gc.spline - spline object of the growth curve
#	dydt.spline - predicted first derivative of the growth curve
#	dydtdt.spline - predicted second derivative of the growth curve
	
# Find possible maximum growth rates - 0s in second derivative

possible.mus <- data.frame()
counter=1
for (a in 1:(length(dydtdt.spline$y)-1)){
	if((sign(dydtdt.spline$y[a])==1)){
		if( (sign(dydtdt.spline$y[a+1])==-1) ){
		Slope <- dydt.spline$y[a]
		OD <- gc.spline$y[a]
		Time <- gc.spline$x[a]
		y.mu.diff <- OD-gc.spline$y[1]
		lambda<- Time-(y.mu.diff/Slope)	
			
		if(lambda <30){
			if(lambda >0){
		possible.mus[counter, "Index" ] <- a
		possible.mus[counter, "Slope"] <- Slope
		possible.mus[counter, "OD"] <- OD
		possible.mus[counter, "Time"] <- Time
		possible.mus[counter, "Lambda"]<- lambda
		counter=counter+1
			}}}} # if goes from pos to neg second deriv
} # for a in 1:number.points


mu.index <- which(possible.mus$Slope == max(possible.mus$Slope))[1]

	
# Now find A after that mu based on the place where growth rate halves.
mu <- possible.mus$Slope[mu.index]
which(dydt.spline$y < (mu*.4)) -> little.growth.index
little.growth.index[which(little.growth.index >= possible.mus$Index[mu.index])] -> little.growth.after.mu
min(little.growth.after.mu) -> A.index

	
mu.all <- list(mu=possible.mus$Slope[mu.index], time.mu=possible.mus$Time[mu.index], y.mu=possible.mus$OD[mu.index], lambda=possible.mus$Lambda[mu.index], time.A = gc.spline$x[A.index], A = gc.spline$y[A.index], index.A = A.index)
} #end function