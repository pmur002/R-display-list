
# Run somewhere that this will not do damage !!!
system("rm *.png")
system("rm *.rds")

source("test-common.R")

# Basic test for recording and replaying 'graphics' plot
# Fails in current R because save/load of recorded plot has been outlawed
graphicsPlot <- function() {
    plot(1)
}

testReload(graphicsPlot, model=graphicsPlot, filestem="graphics-plot")

# Test for being able to ADD to a replayed 'graphics' plot
graphicsAppend <- function() {
    segments(.8, .8, 1.2, 1.2)
}

graphicsAppendModel <- function() {
    plot(1)
    segments(.8, .8, 1.2, 1.2)
}

testReload(graphicsPlot, append=graphicsAppend, model=graphicsAppendModel,
           filestem="graphics-plot-append")

# Test for recording and replaying 'grid'-based plot
# Really just testing the graphics engine again
latticePlot <- function() {
    require(lattice)
    xyplot(1 ~ 1)
}

testReload(latticePlot, model=latticePlot, filestem="lattice-plot")

# Test for being able to ADD to a replayed 'grid' plot
# Tests BOTH viewports and grobs
gridPlot <- function() {
    require(grid)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
}

gridAppend <- function() {
    require(grid)
    grid.circle()
    grid.edit("r", gp=gpar(col="red"))
}

gridAppendModel <- function() {
    require(grid)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))
}

testReload(gridPlot, append=gridAppend, model=gridAppendModel, filestem="grid")

# Same test, but with 'grid' drawing already on device
gridPrepend <- function() {
    require(grid)
    grid.segments()
}

gridPrependModel <- function() {
    require(grid)
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
    plot(1)
}

graphicsPrependAppendModel <- function() {
    plot(1)
    plot(1)
    segments(.8, .8, 1.2, 1.2)    
}

testReload(graphicsPlot, prepend=graphicsPrepend, append=graphicsAppend,
           model=graphicsPrependAppendModel, filestem="graphics-prepend")
     
# Test 'grid', but with 'graphics' drawing already on device
# NOTE that this will just FAIL without plot.new() at start of saved DL
# "display list redraw incomplete" then "invalid graphics state"
# (because 'graphics' GE_RestoreState sets 'graphics' state to 0)
# Separate bug to fix ?
graphicsGrid <- function() {
    plot.new()
    require(grid)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
}

graphicsGridModel <- function() {
    plot(1)
    require(grid)
    # Replay of display list will force a new page
    # This may be just propagating another bug (?)
    grid.newpage()
    plot.new()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))    
}

testReload(graphicsGrid, prepend=graphicsPrepend, append=gridAppend,
           model=graphicsGridModel, filestem="graphics-grid-prepend")


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
    require(grid)
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
