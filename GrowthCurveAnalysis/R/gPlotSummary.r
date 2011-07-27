gPlotSummary <- function(all.top.averages, all.info, average.categories, parameters){
# This function, called by gSummaryPlots, makes a plot which summarises the entirety of the analysis object passed to gSummaryPlots. It is printed in a separate pdf file.
#
# Args
#	all.top.averages - all Level.1 averages
#	all.info - a dataframe with length(analysis) entries containing database metainformation for each curve and groupings for each level.
#	average.categories - the combination of user-defined also.average and default categories to average (well.number, well.name etc) split into levels whic correspond to the stages of averaging.
# 	parameters - a list (should be about 4 in length) of all the parameters to show at the bottom of the graphs. 

# Returns
#	prints a graph, doesn't return anything.

# Find number of curves to include on the plt
number.curves <- length(unique(all.info[[paste("Level.", 1, sep="")]]))
colors <- rainbow(number.curves)

# And all the categories
all.categories <- names(all.info)

# Find the attributes which are all the same throughout all curves and haven't already been purposefully ignored (as in also.average). Only the defining attributes will be used in the legend.

#Find a list of ignored categories.
all.cats.ign = NULL

for (a in 1:length(average.categories)){
all.cats.ign <- append(all.cats.ign, average.categories[[a]])	
} #

#Add the levels to the list of categories which don't matter
for (b in 1:length(average.categories)){
	all.cats.ign <- append(all.cats.ign, paste("Level.", b, sep=""))	
} #for b
	
# If a categories has not been explicitly ignored, add it to the "to mention" list of categories (to mention if they are all the same throughout or in the legend if they define curves)
categories.not.ignored <- NULL

for (c in 1:length(all.categories)){
	if ((all.categories[c] %in% all.cats.ign) == FALSE){
		categories.not.ignored <- append(categories.not.ignored, all.categories[c])
	} #if not a category to be ignored
} # for c in all cats	
	
	
# If the attributes are all the same in any given category
inconsistent.cats <- NULL
title.info <- NULL
for (d in 1:length(categories.not.ignored)){
	if (length(unique(all.info[[categories.not.ignored[d]]])) == 1){
	title.info <- paste(title.info, gsub(pattern="concentration", replacement="", x=categories.not.ignored[d], ignore.case=TRUE), ":", all.info[[categories.not.ignored[d]]][1], " | ", sep="")
	} else {#if only one result	
	inconsistent.cats <- append(inconsistent.cats, categories.not.ignored[d])
	}# add to differentiating category 
} #for d in each category.

# Find legend info for each curve
legend.labels <- NULL
for (e in 1:number.curves){
	legend.entry <- NULL
	# Find a representative curve
	repr.curve <- which(all.info[[paste("Level.", 1, sep="")]]==e)[1]
	for (f in 1:length(inconsistent.cats)){
		if (is.na(all.info[repr.curve,inconsistent.cats[f]])==FALSE){
			legend.entry <- paste(legend.entry, " | ",  gsub(pattern="concentration", replacement="", x=inconsistent.cats[f], ignore.case=TRUE), ":", all.info[repr.curve,inconsistent.cats[f]], sep="")
		} #if interesting attribute (not NA)
	} #for f in inconsentent cats
	legend.labels <- append(legend.labels, legend.entry)
} # for e

######################## Begin plotting.
#list of consistent cats, name is the cat, vector is the list
# list of list by well of defining cats, equal length vectors with attributes and widths. 
# all top averages with number.curves curves (averages)
# know top level group number (number.curves)
	
# Find widths needed in graph for legend etc.
legend.width <- convertWidth(unit(max(strwidth(legend.labels, "inches"))*.75, "inches")+unit(.05, "inches"), "inches")

# Actual Graphing

background <- viewport(width=unit(11, "inches"), height=unit(8.5, "inches"), layout=grid.layout(nrow=3, heights=unit(c(.4,6.4, 1.7), "inches"), ncol = 2, widths=unit.c(unit(1, "null"), legend.width)))
pushViewport(background)
grid.rect()

# Title

title.text.size <- 15
while((unit(strwidth(title.info, "inches", cex=title.text.size), "inches")[[1]])> 11) {
	title.text.size <- title.text.size*.95
	}
title.text.size <- title.text.size*.85

title <- viewport(layout.pos.col=c(1,2), layout.pos.row=1 )
pushViewport(title)
grid.text(paste("Full Summary", title.info), just="top", y=unit(.6, "npc"), gp=gpar(cex=title.text.size, col="black"))
popViewport() #title

# Graph

# Axis lengths
y.axis.max <- 0
x.axis.max <- 0
for (g in 1:number.curves){
	y.axis.max <- max(y.axis.max, all.top.averages[[g]]$spline$y)
	x.axis.max <- max(x.axis.max, all.top.averages[[g]]$spline$x)
} #for e

pushViewport(viewport(layout.pos.col=1, layout.pos.row=2)) #parent graph viewport
graph <- plotViewport(margins = c(2, 3.5, 1, 1))
pushViewport(graph)

grid.rect(gp=gpar(col="blue"))
raw.data <- dataViewport(xscale=c(0,x.axis.max), yscale=c(0,1.05*y.axis.max)) 
pushViewport(raw.data)

grid.rect()
grid.xaxis(gp=gpar(fontsize=8))
grid.yaxis(gp=gpar(fontsize=8))
grid.text("Time (in hours)", y=unit(-2, "lines"), gp=gpar(fontsize=10))
grid.text("Optical Density", x=unit(-3, "lines"), gp=gpar(fontsize=10), rot=90)


for (h in 1:number.curves){
grid.lines(x=all.top.averages[[h]]$spline$x, y=all.top.averages[[h]]$spline$y, default.units="native", gp=gpar(col=colors[h], lwd=2))
} #for j

popViewport() #data
popViewport() #graph
popViewport() # parent graph viewport

# Legend

v.legend <- viewport(layout.pos.col=2, layout.pos.row=2, layout=grid.layout(ncol=2, widths <- unit.c(unit(.3, "inches"), unit(1, "null")), nrow=number.curves*2))
pushViewport(v.legend)
for (i in 1:number.curves){
	# For the first row of the legend entry
	pushViewport(viewport(layout.pos.col=1, layout.pos.row=(2*i-1)))
	grid.rect(gp=gpar(fill=colors[i]))
	popViewport()
	pushViewport(viewport(layout.pos.col=2, layout.pos.row=(2*i-1)))
	grid.text(paste("Level 1: Group", i, " _ ", length(which(all.info[[paste("Level.", 1, sep="")]]==i)), " curves", sep=""), gp=gpar(fontsize=10, col="blue"))
	grid.rect()
	popViewport()
	pushViewport(viewport(layout.pos.col=1, layout.pos.row=(2*i)))
	grid.rect(gp=gpar(fill=colors[i]))
	popViewport()
	pushViewport(viewport(layout.pos.col=2, layout.pos.row=(2*i)))
	grid.text(legend.labels[i], gp=gpar(fontsize=8))
	popViewport()
} # for i in each curve
popViewport() #v.legend


# Parameters
v.parameters <- viewport(layout.pos.col=c(1,2), layout.pos.row=3, layout=grid.layout(ncol=length(parameters)))
pushViewport(v.parameters)

for (j in 1:length(parameters)){
	# Gather average of each curve and running average.
	parameter.points <- NULL
	to.average <- NULL
	for (k in 1:number.curves){
		to.average <- append(to.average, all.top.averages[[k]]$parameters[[parameters[j]]])
		parameter.points <- append(parameter.points, mean(all.top.averages[[k]]$parameters[[parameters[j]]]))
	} #for each ccurve
	
	average <- mean(to.average)
	standard.dev <- sd(to.average)
	
	pushViewport(viewport(layout.pos.col=j))
	grid.rect(gp=gpar(col="black"))
	pushViewport(plotViewport(margins = c(3, 1, 1, 1)))
	
		
	min <- min(parameter.points)
	max <- max(parameter.points)
	
	if (max != min){
	scale.min <- min - .05*max+.05*min
	scale.max <- max + .05*max -.05*min
	} else {
	scale.min <- (.95*max)
	scale.max <- (1.05*max)
	}
	
	raw.data <- dataViewport(parameter.points, 1, xscale=c(scale.min, scale.max), yscale=c(0,2))
	pushViewport(raw.data)
	
	# Make axis for parameters
	grid.xaxis(gp=gpar(fontsize=8))
	grid.segments(x0=unit(scale.min, "native"), x1=unit(scale.max, "native"), y0=unit(0, "native"), y1=unit(0, "native"))
	
	# Label x-axis
	grid.text(parameters[j], y=unit(-2, "lines"), gp=gpar(fontsize=10))
	grid.text(paste("Average=", signif(average, 3), " | SD=", signif(standard.dev,3), sep=""), y=unit(-4, "lines"), gp=gpar(fontsize=7, col="gray26"))
	
	# Add a vertical line for the average
	grid.segments(x0=unit(average, "native"), x1=unit(average, "native"), y0=unit(.2, "native"), y1=unit(1.8, "native"), gp=gpar(col="gray26", lwd=2))
		
	# Plot the value of each parameter for each curve in a matching color.	
	for (l in 1:number.curves){
	grid.points(x=parameter.points[l], y=1, default.units="native", pch=4, gp=gpar(col=colors[l], cex=.5))
	}
	
	popViewport() # raw.data
	popViewport() #outer
	popViewport() #plot viewport
	
} #for j in parameters

popViewport() #v.parameters


popViewport() #background

}# end plot