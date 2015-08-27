
# Run somewhere that this will not do damage !!!
system("rm *.png")
system("rm *.rds")

source("test-common.R")

# Basic test for recording and replaying 'graphics' plot
# Fails in current R because save/load of recorded plot has been outlawed
graphicsPlot <- function() {
    plot(1, col="red")
}

testReload(graphicsPlot, model=graphicsPlot, filestem="graphics-plot")

# Test for being able to ADD to a replayed 'graphics' plot
graphicsAppend <- function() {
    segments(.8, .8, 1.2, 1.2)
}

graphicsAppendModel <- function() {
    plot(1, col="red")
    segments(.8, .8, 1.2, 1.2)
}

testReload(graphicsPlot, append=graphicsAppend, model=graphicsAppendModel,
           filestem="graphics-plot-append")

# Test for recording and replaying 'grid'-based plot
# Really just testing the graphics engine again
latticePlot <- function() {
    require(lattice, quietly=TRUE)
    xyplot(1 ~ 1)
}

testReload(latticePlot, model=latticePlot, filestem="lattice-plot")

# Test for being able to ADD to a replayed 'grid' plot
# Tests BOTH viewports and grobs
gridPlot <- function() {
    require(grid, quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
}

gridAppend <- function() {
    require(grid, quietly=TRUE)
    grid.circle()
    grid.edit("r", gp=gpar(col="red"))
}

gridAppendModel <- function() {
    require(grid, quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))
}

testReload(gridPlot, append=gridAppend, model=gridAppendModel, filestem="grid")

# Test for being able to ADD to a replayed 'lattice' plot
latticeAppend <- function() {
    require(grid, quietly=TRUE)
    downViewport("plot_01.panel.1.1.vp")
    grid.segments()
}

latticeAppendModel <- function() {
    require(lattice, quietly=TRUE)
    xyplot(1 ~ 1)
    require(grid, quietly=TRUE)
    downViewport("plot_01.panel.1.1.vp")
    grid.segments()
}

testReload(latticePlot, append=latticeAppend, model=latticeAppendModel,
           filestem="lattice-append")

# Test for being able to ADD to a replayed 'grid' plot,
# but with 'grid' drawing already on device
gridPrepend <- function() {
    require(grid, quietly=TRUE)
    grid.segments()
}

gridPrependModel <- function() {
    require(grid, quietly=TRUE)
    grid.segments()
    # Replay of display list will force a new page
    grid.newpage()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))    
}

testReload(gridPlot, prepend=gridPrepend, append=gridAppend,
           model=gridPrependModel, filestem="grid-prepend")

# Test 'graphics' replay with 'graphics' already on device
graphicsPrepend <- function() {
    plot(1, col="green")
}

graphicsPrependAppendModel <- function() {
    plot(1, col="green")
    plot(1, col="red")
    segments(.8, .8, 1.2, 1.2)    
}

testReload(graphicsPlot, prepend=graphicsPrepend, append=graphicsAppend,
           model=graphicsPrependAppendModel, filestem="graphics-prepend")
     
# Test 'grid', but with 'graphics' drawing already on device
graphicsGrid1 <- function() {
    require(grid, quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
}

graphicsGridModel1 <- function() {
    plot(1, col="green")
    require(grid, quietly=TRUE)
    # Replay of display list will force a new page
    grid.newpage()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))    
}

testReload(graphicsGrid1, prepend=graphicsPrepend, append=gridAppend,
           model=graphicsGridModel1, filestem="graphics-grid-prepend-1")

# Test mix of 'graphics' and 'grid', with 'graphics' drawing already on device 
graphicsGrid2 <- function() {
    plot.new()
    require(grid, quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
}

graphicsGridModel2 <- function() {
    plot(1, col="green")
    require(grid, quietly=TRUE)
    plot.new()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))    
}

testReload(graphicsGrid2, prepend=graphicsPrepend, append=gridAppend,
           model=graphicsGridModel2, filestem="graphics-grid-prepend-2")

# Demonstration of the fact that the 'grid' DL is NOT erased when
# 'graphics' starts a new page, SO the 'grid' DL is included in
# the "recordedplot" and restored on replay (so 'grid' is involved
# in the replay because it thinks that there is 'grid' output on
# the engine DL [even though there is not];  a symptom of this
# involvement could be the injection of an unnecessary [blank] new page!)
gridDLgraphics <- function() {
    require(grid, quietly=TRUE)
    grid.segments()
    plot(1, col="red")
}

gridDLgraphicsModel <- function() {
    require(grid, quietly=TRUE)
    grid.segments()
    plot(1, col="red")
    plot(1, col="red")
}

testReplay(gridDLgraphics, model=gridDLgraphicsModel,
           filestem="grid-DL-graphics")

# This demonstration gets weirder;  we modify a grob on the 'grid' DL
# that is NOT visible on screen!
# Record/replay does work though (!)
gridDLgraphicsEdit <- function() {
    require(grid, quietly=TRUE)
    grid.segments(name="s")
    plot(1, col="red")
    grid.edit("s", gp=gpar(col="red"))
}

gridDLgraphicsEditModel <- function() {
    require(grid, quietly=TRUE)
    grid.segments(name="s")
    plot(1, col="red")
    grid.edit("s", gp=gpar(col="red"))
    # Replay of display list will force a new page
    grid.newpage()
    grid.segments(gp=gpar(col="red"))
}

testReplay(gridDLgraphicsEdit, model=gridDLgraphicsEditModel,
           filestem="grid-DL-graphics-edit")

# Tests for recording/replaying arbitrary R code on DL (recordGraphics)
# (pretty much any 'grid' code will do this anyway?)


# Tests for recording/replaying when there is NO 'grid' output


# Tests for COPYING from one device to another
testCopy(gridPlot, append=gridAppend, model=gridAppendModel,
         filestem="grid-copy")

# Tests for copying BY REFERENCE
# (take copy of grid DL, modify grid DL, replay grid DL ...
#  ... get changes in replay?! [which would be bad!] )
# NO problem (because grid.edit() creates new object in old DL ?)
gridEdit <- function() {
    grid.edit("r", gp=gpar(col="red"))
}

gridEditModel <- function() {
    require(grid, quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.edit("r", gp=gpar(col="red"))
    # Replay of display list will force a new page
    grid.newpage()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")    
}

testReplay(gridPlot, prepend=gridEdit, model=gridEditModel,
           filestem="grid-edit")


# Tests with graphics engine DL OFF !
# (so redrawing is entirely up to 'grid')


# Tests with 'grid' DL OFF !?


# Test across R versions
testReload(graphicsPlot, model=graphicsPlot, filestem="graphics-plot-R-version",
           testVersion=TRUE)

# Test across R versions, grid on DL
testReload(latticePlot, model=latticePlot, filestem="lattice-plot-R-version",
           testVersion=TRUE)

# Test with no graphics system packages loaded
testReload(graphicsPlot, model=graphicsPlot, filestem="graphics-plot-no-graphics",
           defaultPackages=NULL)

# Test with 'grid' loaded before 'graphics'
testReload(graphicsPlot, model=graphicsPlot, filestem="graphics-plot-no-graphics",
           defaultPackages=c("grid", "graphics"))
