
gSummarySlice <- function(all.prev.level, level, group, compare, all.param, control, group.info, prev.level.name=prev.level.name, categories.look){
# Makes views of slices of parameter space for each group and each level in gSummaryPlots

num.ccurves <- length(unique(group.info[[prev.level.name]]))
ccurves <- unique(group.info[[prev.level.name]])
# Colors
colors <- rainbow(num.ccurves)
# Graph Title
title.text <- paste("Slices - Level ", level, ": Grouping ", group, sep="")

grid.newpage()

# Legend and points (colored to match legend.
legend.info <- NULL
param.plot <- list()
for (c in ccurves){
	new.entry <- NULL
	#Find a representative of each ccurve.
	repr.ccurve <- which(group.info[[prev.level.name]]==c)[1]
	for (d in 1:length(categories.look[[(level+1)]])){
		new.entry <- paste(new.entry, categories.look[[level+1]][d], ": ", group.info[repr.ccurve,categories.look[[level+1]][d]], sep="")
	} #for d in every category to look at
	legend.info <- append(legend.info, new.entry)
	
	# Get info for param
	for (e in 1:length(compare)){
	param.plot[[compare[e]]][[c]] <- all.prev.level[[c]]$parameters[[compare[e]]]
	} # for e in 1:length(compare)
} #for c

num.box <- length(compare)
slice.param <- list()
for  (f in 1:length(compare)){ 
slice.param[[compare[f]]] <- list(data=as.numeric(all.param[[compare[f]]]),
	scale = c(min(as.numeric(all.param[[compare[f]]]))*.9, max(as.numeric(all.param[[compare[f]]]))*1.1),
	name=compare[f])
} # for each param



background <- viewport(width=unit(7, "inches"), height=unit(7, "inches"), layout=grid.layout(nrow=num.box+2, ncol=num.box+2, 
		widths=unit.c(unit(3.5, "lines"), rep(unit(1, "null"), times=num.box), unit(1, "lines")), 
		heights=unit.c(unit(3, "lines"), rep(unit(1, "null"), times=num.box), unit(1, "lines"))))
		
pushViewport(background)
grid.rect()

#Grid y-axis labels:

for (g in 1:num.box){
	
	pushViewport(viewport(layout.pos.row=g+1, layout.pos.col=1))
grid.text(compare[g], just=c("center", "center"), x=unit(.6, "lines"), y=unit(.5, "npc"), rot=90, gp=gpar(fontsize=10))
popViewport()
} # for each compare g to grid y-axis


# Make Slices
for (h in 1:num.box){ # for cols
	for (i in h:num.box){ # for rows
		current.xaxis <- slice.param[[compare[h]]]
		current.yaxis <- slice.param[[compare[i]]]
	
		box <- viewport(layout.pos.row=i+1, layout.pos.col=h+1)
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


		# Grid the background points:
		grid.points(x=current.xaxis$data, y=current.yaxis$data,size=unit(.01, "inches"), pch=19, gp=gpar(col="black"))

		# Grid the interesting points:
		color.counter=0
		for (k in ccurves){ #for each color set of points# for each point
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
pushViewport(viewport(layout.pos.row=m, layout.pos.col=m+1))
grid.text(compare[m], just=c("center", "center"), x=unit(.5, "npc"), y=unit(.5, "npc"), gp=gpar(fontsize=10))
popViewport()
} # for each column





legend.side <- floor(length(slice.param)*.5)
legend <- viewport(layout.pos.col=c((num.box+2-legend.side):(num.box+2)), 
	layout.pos.row=c(2:(legend.side)),
	layout=grid.layout(ncol=2, nrow=num.ccurves, widths=unit.c(unit(.5, "inches"), unit(1, "null"))))
pushViewport(legend)

grid.rect(gp=gpar(lwd=2))


for (d in 1:num.ccurves){
pushViewport(viewport(layout.pos.row=d, layout.pos.col=1))
grid.roundrect(just=c("centre", "centre"), gp=gpar(fill=colors[d]))
popViewport()

pushViewport(viewport(layout.pos.row=d, layout.pos.col=2, just="left"))
grid.text(paste(legend.info[d]), gp=gpar(fontsize=10))#as.character(color.info[d]))
popViewport()
}


#TODO: add legendy stuff, isn't gridding the right area - maybe this is better?
popViewport() #legend


popViewport()#background

}
#END FUNCTION