###### run:
# gSummaryPlot(analysis, also.average.categories=list(c("Technical.Replicate"), c("Biological.Replicate"), c("Date")), control=gControl()) -> test
# 

#BGN FUNCTION
gSummaryPlots <-function(analysis, also.average.categories = list(c("Technical.Replicate"), c("Biological.Replicate"), c("Date")), redefine.average.categories=NULL, parameters=c("A", "mu", "lambda", "integral"), control=gControl()) {
# Groups plots by matching metadata and averages as demanded (technical/biological replicates), providing a series of graphs - two rounds of averaging probably.

# Args
#	analysis - an object of class "gAnalysis" containing information about a set of wells, as produced by gChoose and gAnalysis. 
#	also.average.categories - categories of metadata to iteratively ignore, first item in list is the first round of ignoring.
#	redefine.average.categories - see the initial definition of average categories below. Gives the user the option to override from outside of the function if these should not be averaged.
#	parameters - a list of the names of the parameters to include on the summary graphs.
#	control - a object of class gControl cotaining global information 
#		$export.folder - folder where pdfs are saved.


# Returns - parameters useful to making similar graphs outside of this function set.
#	all.info - a data.frame with as many entries as curves in analysis, containing all the metainformation and grouping information.
#	all.averages - a list with sublists Level.1, Level.2, etc, each with a list of averaged curves at that level (the first one relates to grouping 1) in a skeletal gAnalysis style object.

#TODO: 
#  FLAG - difficulties with NA in comparison to see if equal. Just changed NA to a character string, so expected NA <=> NA the only positive match

## Error checking/getting ready

# Load the Grid package. TODO: Add a check to see whether package is already loaded first, or error if it can't be loaded
library(grid)

# TODO: Add error check that analysis is of class "gAnalysis"

## Prepare the averaging stipulations.

# Some defaults for the categories to average:
average.categories.default <- c("File.Name", "Well.Name", "Well.Number", "Temp", "gAnalysisIndex")

#This means, consider graphs comperable and average them if everything if every attribute is the same in every category except the ones listed. gAnalysisIndex is introduced later in this function as a bookkeeping number and should of course be ignored in comparisons.

if (is.null(redefine.average.categories) == FALSE){
average.categories <- redefine.average.categories
graph.category.last.level <- average.categories[[1]]
} else {
average.categories <- also.average.categories
average.categories[[1]] <- append(average.categories[[1]], average.categories.default)
graph.category.last.level <- also.average.categories[[1]]
num.levels <- length(average.categories)
} # end if redefine.average.categoires is not NULL

## Read the metainformation from analysis into a more useable dataframe. Make a similar data.frame for each parameters
all.info <- data.frame(gAnalysisIndex=c(1:length(analysis)), stringsAsFactors =FALSE)
all.param <- data.frame(gAnalysisIndex=c(1:length(analysis)), stringsAsFactors =FALSE)
for (a in 1:length(analysis)){
	info.names = names(analysis[[a]]$Info)
	for (b in 1:length(info.names)){
		all.info[a, info.names[b]] <- as.character(analysis[[a]]$Info[[info.names[b]]])
	} # for b, each name of info
	param.names = names(analysis[[a]]$parameters)
	for (b in 1:length(param.names)){
		all.param[a, param.names[b]] <- as.character(analysis[[a]]$parameters[[param.names[b]]])
	} # for b, each name of info
}# for a, for every well.

# Replace all the NAs with "NA"s so they don't give trouble when judging whether they're the same - i.e. assume all NAs mean the same thing.
all.info[is.na(all.info)] <- "NA"

## Given categories to be averaged and all the categories in all the wells, find which categories must be the same. I.e. any category not in average.categories must be in categories.diff (categories which are used to differentiate between curve families, i.e. Media is probably in categories.diff)

all.categories <- names(all.info)
categories.diff <- list()

for (b in 1:num.levels){
	counter=0
	average.cats=NULL
	for (d in 1:b){average.cats <- append(average.cats, average.categories[[d]])
	} #for d in each level up until then.
	
for (c in 1:length(all.categories)){
	
	if((all.categories[c] %in% average.cats) == FALSE){
	
	counter=counter+1
	if (counter == 1){
	categories.diff[[b]] <- all.categories[c]	
	} else {#if first addition 
	categories.diff[[b]] <- append(categories.diff[[b]], all.categories[c])
	}} #else
} #for c in all categories
} #for b in levels

categories.look=list()
categories.look[[1]] <- categories.diff[[num.levels]]

if (num.levels != 1){ for (b in 2:num.levels){ categories.look[[b]] <- average.categories[[num.levels-b
+2]]}}

# left over for graphing
categories.look[[num.levels+1]] <- graph.category.last.level

## Iteritavely group wells in all.info by appending numbers refering to the level of organization
count.prev.divis <- 1 

for (e in 1:num.levels){
	
	count.divis <- 0
	
	for (f in 1:count.prev.divis){
	if (count.prev.divis==1) {remaining.wells <- all.info[["gAnalysisIndex"]]
	} else {remaining.wells <- all.info$gAnalysisIndex[all.info[[paste("Level.", e-1, sep="")]] == f]
	} # if previsious divis is 1
	
	
	while (length(remaining.wells) >= 1){
		all.info[remaining.wells[1],] -> match.well #choose the first well in the category that has not yet been matched to match
		matching.wells <- remaining.wells[1]
		
		if (length(remaining.wells) != 1){
			count.divis <-  count.divis +1
			for (g in 2:length(remaining.wells)){
				score <- 0
				for (h in 1:length(categories.look[[e]])){
					if (match.well[[categories.look[[e]][h]]] != all.info[remaining.wells[g], categories.look[[e]][h]]){
						score <- 1
					}# if in that category the wells aren't equal
					#TODO: Deal with NAs
				}#for each category to look at
				if (score == 0){
					matching.wells<- append(matching.wells, remaining.wells[g])
				}# if all matched that mattered
			}# for g in the rest of the remaining wells
		
		all.info[remaining.wells,paste("Level.", e, sep="")] <- count.divis
		remaining.wells <- remaining.wells[!remaining.wells %in% matching.wells] #remove wells which have been matched from the remaining wells to match
		}# as long as there's more than 1 well left
	} #while wells remain to be sorted
	} #for f in 1:count prev divis
	
	count.prev.divis <- count.divis
	
} #for e in num.levels


all.averages <- list()

# Average curves for groupings and graph

pdf(file=paste(control$export.folder, "/", "gAnalysisGraphs_changename.pdf", sep=""), width=11, height=8.5)


for (f in num.levels:1){ # work backwards so the needed average curves are available to make the next plot
	all.level <- list()
	level.name <- paste("Level.", f, sep="")
	if (f < num.levels) {prev.level.name <- paste("Level.", f+1, sep="")} else {prev.level.name="gAnalysisIndex"}
	for (g in 1:max(all.info[[level.name]])){
		wells.to.average <- all.info$gAnalysisIndex[which(all.info[[level.name]] == g)]
		group.info <- all.info[which(all.info[[level.name]] == g),]
		all.level[[g]] <- gAverageAnalysis(analysis[wells.to.average])
		#browser()
		if (f < num.levels) {
			gPlotAverage(average.curve = all.level[[g]], group.info = group.info, group=g, all.prev.level=all.averages[[prev.level.name]], level=f, categories.look = categories.look, prev.level.name=prev.level.name, parameters=parameters)
			gSummarySlice(all.prev.level=all.averages[[prev.level.name]], level=f, group=g, compare=parameters, all.param=all.param, control=control, group.info=group.info, prev.level.name=prev.level.name, categories.look=categories.look)
			#browser()
			} else {
			gPlotAverage(average.curve = all.level[[g]], group.info = group.info, group=g, all.prev.level=analysis, level=f, categories.look = categories.look, prev.level.name = prev.level.name, parameters=parameters)
			gSummarySlice(all.prev.level=analysis, level=f, group=g, compare=parameters, all.param=all.param, control=control, group.info=group.info, prev.level.name=prev.level.name, categories.look=categories.look)
			}
	} #for each grouping, g in a level
	all.averages[[level.name]] <- all.level
}# for each level

dev.off()


#Single summarising graph:

pdf(file=paste(control$export.folder, "/", "gAnalysisGraphSummary_changename.pdf", sep=""), width=11, height=8.5)
gPlotSummary(all.top.averages = all.averages[[paste("Level.",1, sep="")]], all.info =all.info, average.categories=average.categories, parameters=parameters)

grid.newpage()
gSummarySliceTop(all.top.averages=all.averages[[paste("Level.",1, sep="")]], compare=parameters, control=control, all.info=all.info, average.categories=average.categories, all.param=all.param)
dev.off()

return(list(all.info = all.info, all.averages = all.averages))	
} # end function
#END FUNCTION

