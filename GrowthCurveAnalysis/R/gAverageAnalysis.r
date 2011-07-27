
#BGN FUNCTION
gAverageAnalysis <-function(curves) {
# Averages lists of gAnalysis class or similar. Parameters are concatenated into a string containing all parameters, raw.data from every spline combined onto a single graph and refitted again using spline. Quality of fit is found using a *** method between the new raw data and the smooth.spline average. TODO: add quality of fit information.

# Args
#	curves - an object of class gAnalysis or a list of gAnalysisAverage 

#Returns
#	average.analysis - a spline-like averaging of the curves listed. 


#Error Check
#is this of class gAnalysis or else is each element in the list of class gAnalysisAverage

#Add information from each curve to the average.
average.analysis <- list()
parameters <- list()
max.time <- 10000000000000000

for (a in 1:length(curves)){
	average.analysis$raw.time <- append(average.analysis$raw.time, curves[[a]]$raw.time)
	max.time <- min(max(curves[[a]]$raw.time), max.time)
	average.analysis$raw.data <- append(average.analysis$raw.data, curves[[a]]$raw.data)
	
	parameter.names <- names(curves[[a]]$parameters)
	
	if (a ==1) {
		for (b in 1:length(parameter.names)){
		parameters[[parameter.names[b]]] <- c(curves[[a]]$parameters[[parameter.names[b]]])
		}
	} else {# if a is 1
	
	for (b in 1:length(parameter.names)){
		parameters[[parameter.names[b]]] <- c(parameters[[parameter.names[b]]], curves[[a]]$parameters[[parameter.names[b]]])
	} # for b in 1:num.param
	}# else if a=1
} # for a in length(curves)

#Throw out the time points which are higher then the lowest high...as in which weren't measured in each experiment.

keep <- (average.analysis$raw.time <= (max.time))

average.analysis$raw.time <- average.analysis$raw.time[keep]
average.analysis$raw.data <- average.analysis$raw.data[keep]

average.analysis$spline <- smooth.spline(average.analysis$raw.time, average.analysis$raw.data)

average.analysis$parameters<-parameters

class (average.analysis) <- "gAnalysisAverage"

return(average.analysis)
} #end function
