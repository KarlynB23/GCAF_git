
gSummarySliceTop <- function(all.top.averages, compare, control, all.info, average.categories, all.param){
# Makes a gSlice type graph for the top level graph in gSummaryPlots
#
# Args
#	all.top.averages - a skeletal gAnalysis-like file contiaing information about the top-level averages of all curves in the analysis file.
#	comapare - a vector of parameters to have slices through
#	control - the gControl object passed to the parent function
#	all.info - and info file containing all the meta-info associated with the analysis object passed to gSummaryPlots
#	average.categories - categories to average iteratively in gSummaryPlots
#	all.param - a data.frame similar to all.info containing all the paramter values for each curve


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
param.plot<-list()
for (e in 1:number.curves){
	legend.entry <- NULL
	# Find a representative curve
	repr.curve <- which(all.info[[paste("Level.", 1, sep="")]]==e)[1]
	for (f in 1:length(inconsistent.cats)){
		if (is.na(all.info[repr.curve,inconsistent.cats[f]])==FALSE){
			legend.entry <- paste(legend.entry, " | ",  gsub(pattern="concentration", replacement="", x=gsub(pattern="Background", replacement="", x=gsub(pattern="Media", replacement="", x=inconsistent.cats[f], ignore.case=TRUE), ignore.case=TRUE), ignore.case=TRUE), ":", all.info[repr.curve,inconsistent.cats[f]], sep="")
		} #if interesting attribute (not NA)
	} #for f in inconsentent cats
	legend.labels <- append(legend.labels, legend.entry)
	#browser()
	for (f in 1:length(compare)){
		param.plot[[compare[f]]][[e]] <- all.top.averages[[e]]$parameters[[compare[f]]]
	} # for f in length(compare)
} # for e


num.box <- length(compare)
slice.param <- list()
for  (f in 1:length(compare)){ 
slice.param[[compare[f]]] <- list(data=as.numeric(all.param[[compare[f]]]),
	scale = c(min(as.numeric(all.param[[compare[f]]]))*.9, max(as.numeric(all.param[[compare[f]]]))*1.1),
	name=compare[f])
} # for each param



background <- viewport(width=unit(11, "inches"), height=unit(8, "inches"), layout=grid.layout(nrow=num.box+3, ncol=num.box+2, 
		widths=unit.c(unit(3.5, "lines"), rep(unit(1, "null"), times=num.box), unit(1, "lines")), 
		heights=unit.c(unit(3, "lines"),unit(3, "lines"), rep(unit(1, "null"), times=num.box), unit(1, "lines"))))
		
pushViewport(background)
grid.rect()

pushViewport(viewport(layout.pos.row=1))
grid.text(title.info)
popViewport()#title

#Grid y-axis labels:

for (g in 1:num.box){
	
	pushViewport(viewport(layout.pos.row=g+2, layout.pos.col=1))
grid.text(compare[g], just=c("center", "center"), x=unit(.6, "lines"), y=unit(.5, "npc"), rot=90, gp=gpar(fontsize=10))
popViewport()
} # for each compare g to grid y-axis


# Make Slices
for (h in 1:num.box){ # for cols
	for (i in h:num.box){ # for rows
		current.xaxis <- slice.param[[compare[h]]]
		current.yaxis <- slice.param[[compare[i]]]
	
		box <- viewport(layout.pos.row=i+2, layout.pos.col=h+1)
		pushViewport(box)
		graph <- dataViewport(xscale = current.xaxis$scale, yscale=current.yaxis$scale, xData=current.xaxis$data, yData=current.yaxis$data, default.units="npc")
		pushViewport(graph)
		
			grid.rect()
			
		#Grid x-axis scale if on top:
		if (h==i){
			grid.xaxis(main=F, gp=gpar(fontsize=8)) #puts the x axis at the top
		} #if should do x-axis
		
		# Grid y-axis if in first row:
		if (h==1){
			grid.yaxis(gp=gpar(fontsize=8))
		} #grid y-axis if in first row


		# Grid the interesting points:
		color.counter=0
		for (k in 1:number.curves){ #for each color set of points# for each point
		color.counter=color.counter+1
				grid.points(x=param.plot[[compare[h]]][[k]], pch=19, size=unit(.1, "inches"), y=param.plot[[compare[i]]][[k]], gp=gpar(col=colors[color.counter])) #  size=unit(.03, "inches") pch=19,
		} # for each color set of points
		
		
		
		popViewport()#graph
		popViewport()#box
	} #for i in h:num.box
} #for h in num.boxes

###########


#Add text labels for param along x axis
for (m in 1:num.box){ #for each column
pushViewport(viewport(layout.pos.row=m+1, layout.pos.col=m+1))
grid.text(compare[m], just=c("center", "center"), x=unit(.5, "npc"), y=unit(.5, "npc"), gp=gpar(fontsize=10))
popViewport()
} # for each column





legend.side <- floor(length(slice.param)*.5)
legend <- viewport(layout.pos.col=c((num.box+3-legend.side):(num.box+2)), 
	layout.pos.row=c(2:(legend.side+2)),
	layout=grid.layout(ncol=2, nrow=number.curves, widths=unit.c(unit(.2, "inches"), unit(1, "null"))))
pushViewport(legend)

grid.rect(gp=gpar(lwd=2))


for (d in 1:number.curves){
pushViewport(viewport(layout.pos.row=d, layout.pos.col=1))
grid.roundrect(just=c("centre", "centre"), gp=gpar(fill=colors[d]))
popViewport()

pushViewport(viewport(layout.pos.row=d, layout.pos.col=2, just="left"))
grid.text(paste(legend.labels[d]), gp=gpar(fontsize=6))#as.character(color.info[d]))
popViewport()
}


#TODO: add legendy stuff, isn't gridding the right area - maybe this is better?
popViewport() #legend


popViewport()#background

}
#END FUNCTION