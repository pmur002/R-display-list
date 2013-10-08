#This is a hypothetical example of what a `capture.plots` function could look like (in similar spirit as `capture.output`).
#Something very similar is already available in the evaluate package, but it relies on recordPlot, hooks and hacks.
#Perhaps all that is needed is a more reliable/native implementation of this?


#arbitrary function or script that creates zero or more plots (2 in this case)
myfun <- function(){
  plot(iris)
  plot(cars, main = "Stopping Distance versus Speed")
}

#capture.plots would return a list with plot objects, one for each 'page'
myplots <- capture.plots(myfun())

#it should be serializable 
saveRDS(myplots, "myplots.rds")

#and unserialize in current OR new session:
newplots <- readRDS(myplots)

#continue working in the restored plot
newplot <- capture.plots({
  print(myplots[[2]])
  lines(stats::lowess(cars))
})

#render to png
png()
print(newplot)
dev.off()

#render to pdf
pdf()
print(newplot)
dev.off()

#render to svg
svg()
print(newplot)
dev.off()

