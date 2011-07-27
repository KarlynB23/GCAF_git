#BGN Function

gPlotAverage<- function(average.curve, group.info, group, all.prev.level, level, categories.look, prev.level.name, parameters){
# Plots a graph in the pdf file the component curves and current average curve. Called in gSummary to make plots of every level.

# Args
#	average.curve - a skeletal gAnalysis type object including the spline and parameters for the average of grouping/level on hand (the black curves)
#	all.info - data.frame containing information about metadata and groupings for each curve
#	all.prev.level - list of objects similar to average.curve, to make the component curves for each graph (the rainbow colored curves)
#	level- the level currently being graphed
#	categories.look - a manipulation of average.categories which contains categories to be considered at each phase of the program - basically the ones to not be ignored then the ones to be ignored.
#	prev.level.name - the name (i.e. Level.2) of the previous level
#	parameters - passed from the user, a list (about 4 in length) of parameters to show along the bottom of the graphs
#
# Returns
# a summary plot which is then plotted as a page of gSummaryGraphs.pdf

# Find the number of supporting curves - i.e. the number of sub-curves to show in the graph.
num.ccurves <- length(unique(group.info[[prev.level.name]]))
ccurves <- unique(group.info[[prev.level.name]])
# Colors
colors <- rainbow(num.ccurves)
# Graph Title
title.text <- paste("Summary - Level ", level, ": Grouping ", group, sep="")

grid.newpage()
#plot.new() What does this do anyway?

# Graph right bar - contains the Consistent Metainformation, grouped by level.
error <- NULL
similar.info.bar <- list()
num.sidebar.rows = 0
for (a in 1:(length(categories.look)-1)){
	sim.inf <- NULL
	num.sidebar.rows = num.sidebar.rows+1 # a row to say the level a group
	if (a <= level){
	for (b in 1:length(categories.look[[a]])){
		if (group.info[[categories.look[[a]][b]]][1] != "NA") {sim.inf <- append(sim.inf, paste(categories.look[[a]][b], ": ", group.info[[categories.look[[a]][b]]][1], sep=""))
		num.sidebar.rows = num.sidebar.rows+1
		}
		# error check
		for (z in 1:dim(group.info)[1]){
		if (group.info[[categories.look[[a]][b]]][1] != group.info[[categories.look[[a]][b]]][z]){
			error <- append(error, categories.look[[a]][b])
		}#if
		}#for z

	} #for each new category similar in each level
	} else { #if a less then level
	sim.inf <-  paste("Groupings: ", min(group.info[[paste("Level.", a, sep="")]]), "-", max(group.info[[paste("Level.", a, sep="")]]),sep="")
	 num.sidebar.rows = num.sidebar.rows+1
	} #else if a is bigger than level
	if (is.null(sim.inf) == FALSE){
	similar.info.bar[[a]] <- sim.inf
	} else {
	sim.inf <- " " ### TODO. Error, when serdar didn't use Bio Rep.
	num.sidebar.rows = num.sidebar.rows+1
	similar.info.bar[[a]] <- sim.inf
	}
} #for each level 

# Legend
legend.info <- NULL
for (c in ccurves){
	new.entry <- NULL
	#Find a representative of each ccurve.
	repr.ccurve <- which(group.info[[prev.level.name]]==c)[1]
	for (d in 1:length(categories.look[[(level+1)]])){
		new.entry <- paste(new.entry, categories.look[[level+1]][d], ": ", group.info[repr.ccurve,categories.look[[level+1]][d]], sep="")
	} #for d in every category to look at
	legend.info <- append(legend.info, new.entry)
} #for c

# Axis lengths
y.axis.max <- 0
x.axis.max <- 0
for (e in 1:length(group.info)){
	examine.curve <- group.info[e,prev.level.name] 
y.axis.max <- max(y.axis.max, all.prev.level[[examine.curve]]$spline$y)
x.axis.max <- max(x.axis.max, all.prev.level[[examine.curve]]$spline$x)
} #for e

# Find the width for the sidebar - comine all things for the sidebar into one character vector to compare
test.width <- NULL
for (f in 1:level){
	test.width <- append(test.width, similar.info.bar[[f]])
} #for f 
max.width <- max(strwidth(test.width, "inches"))

sidebar.width <- convertWidth(unit(max.width, "inches")+unit(.2, "inches"), "inches")


# Find the width for the legend
legend.width <- convertWidth(unit(max(strwidth(legend.info, "inches")), "inches")+unit(.6, "inches"), "inches")

graph.width <- convertWidth(unit(11, "inches")-legend.width-sidebar.width, "inches")

########################################################
#Begin Plotting

background <- viewport(width=unit(11, "inches"), height=unit(8, "inches"), layout=grid.layout(nrow=3, heights=unit(c(1,6,1), "inches"), ncol = 3, widths=unit.c(sidebar.width, graph.width, legend.width)))
pushViewport(background)
grid.rect()

#Title

title <-  viewport(layout.pos.col=2, layout.pos.row=1 )
pushViewport(title)
grid.text(title.text, just="top", y=unit(.6, "npc"), gp=gpar(fontsize=30, col="black"))
popViewport() #title

#Sidebar

sidebar <- viewport(layout.pos.col=1, layout.pos.row=2, layout=grid.layout(nrow=num.sidebar.rows, heights=(unit(1.3, "lines"))))
pushViewport(sidebar)
grid.rect()


counter = 1
for (g in 1:length(similar.info.bar)){
pushViewport(viewport(layout.pos.row=counter))
counter=counter+1
if (g > level){
	grid.text(paste("Level", g), gp=gpar(fontsize=15, col="darkred"))} else {
grid.text(paste("Level", g, ": Group", group.info[1,paste("Level.", g, sep="")]), gp=gpar(fontsize=15, col="darkred"))
	}
popViewport()
	for (h in 1:length(similar.info.bar[[g]])){
		pushViewport(viewport(layout.pos.row=counter))
		counter=counter+1
		grid.text(similar.info.bar[[g]][h])
		popViewport()
}} #for g and h
popViewport() # sidebar

#Graph

pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
graph <- plotViewport(margins = c(2, 3.5, 1, 1))
pushViewport(graph)

grid.rect(gp=gpar(col="blue"))
raw.data <- dataViewport(average.curve$spline$x, average.curve$spline$y, xscale=c(0,x.axis.max), yscale=c(0,1.05*y.axis.max))
pushViewport(raw.data)

grid.rect()
grid.xaxis(gp=gpar(fontsize=8))
grid.yaxis(gp=gpar(fontsize=8))
grid.text("Time (in hours)", y=unit(-2, "lines"), gp=gpar(fontsize=10))
grid.text("Optical Density", x=unit(-3, "lines"), gp=gpar(fontsize=10), rot=90)

grid.lines(x=average.curve$spline$x, y=average.curve$spline$y, default.units="native", gp=gpar(col="black", lwd=3))

for (j in 1:num.ccurves){
grid.lines(x=all.prev.level[[ccurves[j]]]$spline$x, y=all.prev.level[[ccurves[j]]]$spline$y, default.units="native", gp=gpar(col=colors[j], lwd=2))
} #for j

popViewport() #data
popViewport() #graph
popViewport() #parent graph viewport

# Legend

v.legend <- viewport(layout.pos.col=3, layout.pos.row=2, layout=grid.layout(ncol=2, widths <- unit.c(unit(.5, "inches"), unit(1, "null")), nrow=length(legend.info)+1, heights=unit(1, "lines")))
pushViewport(v.legend)
pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
	grid.rect(gp=gpar(fill="black"))
popViewport()
pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
	grid.rect()
	grid.text("Average")
	popViewport()
for (i in 1:length(legend.info)){
	pushViewport(viewport(layout.pos.col=2, layout.pos.row=i+1))
	grid.rect()
	grid.text(legend.info[i])
	popViewport()
	pushViewport(viewport(layout.pos.col=1, layout.pos.row=i+1))
	grid.rect(gp=gpar(fill=colors[i]))
	popViewport()
}
popViewport() #v.legend

# Parameter spread

v.parameters <- viewport(layout.pos.col=c(1,2,3), layout.pos.row=3, layout=grid.layout(ncol=length(parameters)))
pushViewport(v.parameters)

for (j in 1:length(parameters)){
	# Gather average of each curve and running average.
	parameter.points <- NULL
	to.average <- NULL
	for (k in 1:num.ccurves){
		to.average <- append(to.average, all.prev.level[[ccurves[k]]]$parameters[[parameters[j]]])
		parameter.points <- append(parameter.points, mean(all.prev.level[[ccurves[k]]]$parameters[[parameters[j]]]))
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
	
	grid.xaxis(gp=gpar(fontsize=8))
	grid.segments(x0=unit(scale.min, "native"), x1=unit(scale.max, "native"), y0=unit(0, "native"), y1=unit(0, "native"))
	
	grid.text(parameters[j], y=unit(-2, "lines"), gp=gpar(fontsize=10))
	grid.text(paste("Average=", signif(average, 3), " | SD=", signif(standard.dev,3), sep=""), y=unit(-4, "lines"), gp=gpar(fontsize=7, col="gray26"))
	
		grid.segments(x0=unit(average, "native"), x1=unit(average, "native"), y0=unit(.2, "native"), y1=unit(1.8, "native"), gp=gpar(col="gray26", lwd=2))
		
	for (l in 1:num.ccurves){
	grid.points(x=parameter.points[l], y=1, default.units="native", pch=19, gp=gpar(col=colors[l], cex=.5))
	}
	
	popViewport() # raw.data
	popViewport() #outer
	popViewport() #plot viewport
	
} #for j in parameters

popViewport() #v.parameters
popViewport() # background


}
#END FUNCTION