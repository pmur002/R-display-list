
# *** README ***

# Usually run this with system R - it is just directing traffic
# (if you run with, say, r-devel, may get errors because
#  system("Rscript") from, say r-devel, will not include
#  the right .libPath()s, which matters for the 'ggplot2' at the bottom)

# *** README ***

# Run somewhere that this will NOT do DAMAGE !!!
# For me that is ~/Files/Research/Rstuff/DisplayList/Testing/
system("rm *.png")
system("rm *.postscript")
system("rm *.pdf")
system("rm *.svg")
system("rm *.rds")

source("test-common.R")

# Basic test for recording and replaying 'graphics' plot
# Fails in current R because save/load of recorded plot has been outlawed
graphicsPlot <- function() {
    plot(1, col="red")
}

testAll(graphicsPlot, model=graphicsPlot, filestem="graphics-plot")

# Test with non-transparent background
graphicsPlotBG <- function() {
    par(bg="pink")
    plot(1)
}

testAll(graphicsPlotBG, model=graphicsPlotBG, filestem="graphics-plot-bg")

# Test with 'grid' loaded, but no 'grid' drawing
graphicsGridLoaded <- function() {
    require("grid", quietly=TRUE)
    plot(1, col="red")
}

testAll(graphicsGridLoaded, model=graphicsPlot, filestem="graphics-grid-loaded")

# Test for being able to ADD to a replayed 'graphics' plot
graphicsAppend <- function() {
    segments(.8, .8, 1.2, 1.2)
}

graphicsAppendModel <- function() {
    plot(1, col="red")
    segments(.8, .8, 1.2, 1.2)
}

testAll(graphicsPlot, append=graphicsAppend, model=graphicsAppendModel,
        filestem="graphics-plot-append")

# Test for recording and replaying 'grid'-based plot
# Really just testing the graphics engine again
latticePlot <- function() {
    require("lattice", quietly=TRUE)
    xyplot(1 ~ 1)
}

testAll(latticePlot, model=latticePlot, filestem="lattice-plot")

# Test for 'ggplot2'
# This will not work across R sessions UNLESS we reload
# 'ggplot2' in new R session, so use prepend to do that
ggplot2Plot <- function() {
    require("ggplot2", quietly=TRUE)
    ggplot(mtcars) + geom_point(aes(x=disp, y=mpg))
}

ggplot2Prepend <- function() {
    require("ggplot2", quietly=TRUE)
}

# (BUT prepend will not work on different R version test)
testAll(ggplot2Plot, prepend=ggplot2Prepend, model=ggplot2Plot,
        filestem="ggplot2", testVersion=FALSE)

# Test for being able to ADD to a replayed 'grid' plot
# Tests BOTH viewports and grobs
gridPlot <- function() {
    require("grid", quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
}

gridAppend <- function() {
    require("grid", quietly=TRUE)
    grid.circle()
    grid.edit("r", gp=gpar(col="red"))
}

gridAppendModel <- function() {
    require("grid", quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))
}

# CANNOT test recorded plot from old R version because recordedplot
# will not contain grid DL, so append will not work
# (no "r" grob to find)
# The result is multi-page, which makes testing of copy to
# PostScript, PDF, and SVG tricky
testAll(gridPlot, append=gridAppend, model=gridAppendModel, filestem="grid",
        testVersion=FALSE, dev="png")

# Test for being able to ADD to a replayed 'lattice' plot
latticeAppend <- function() {
    require("grid", quietly=TRUE)
    downViewport("plot_01.panel.1.1.vp")
    grid.segments()
}

latticeAppendModel <- function() {
    require("lattice", quietly=TRUE)
    xyplot(1 ~ 1)
    require("grid", quietly=TRUE)
    downViewport("plot_01.panel.1.1.vp")
    grid.segments()
}

# This works even for older R version because the 'grid' viewports are
# recreated by the graphics engine DL replay
testAll(latticePlot, append=latticeAppend, model=latticeAppendModel,
        filestem="lattice-append")

# Test for being able to ADD to a replayed 'grid' plot,
# but with 'grid' drawing already on device
gridPrepend <- function() {
    require("grid", quietly=TRUE)
    grid.segments()
}

gridPrependModel <- function() {
    require("grid", quietly=TRUE)
    grid.segments()
    # Replay of display list will force a new page
    grid.newpage()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))    
}

# Do not test dev.copy() for this one because 'prepend' does not make sense
# CANNOT test recorded plot from old R version because recordedplot
# will not contain grid DL, so append will not work
# (no "r" grob to find)
testAll(gridPlot, prepend=gridPrepend, append=gridAppend,
        model=gridPrependModel, filestem="grid-prepend",
        testCopy=FALSE, testVersion=FALSE)

# Test 'graphics' replay with 'graphics' already on device
graphicsPrepend <- function() {
    # Specify package so this can be tested without 'graphics' loaded
    graphics::plot(1, col="green")
}

graphicsPrependAppendModel <- function() {
    plot(1, col="green")
    plot(1, col="red")
    segments(.8, .8, 1.2, 1.2)    
}

# Do not test dev.copy() for this one because 'prepend' does not make sense
testAll(graphicsPlot, prepend=graphicsPrepend, append=graphicsAppend,
        model=graphicsPrependAppendModel, filestem="graphics-prepend",
        testCopy=FALSE)
     
# Test 'grid', but with 'graphics' drawing already on device
graphicsGrid1 <- function() {
    require("grid", quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
}

graphicsGridModel1 <- function() {
    plot(1, col="green")
    require("grid", quietly=TRUE)
    # Replay of display list will force a new page
    grid.newpage()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))    
}

# Do not test dev.copy() for this one because 'prepend' does not make sense
# CANNOT test recorded plot from old R version because recordedplot
# will not contain grid DL, so append will not work
# (no "r" grob to find)
testAll(graphicsGrid1, prepend=graphicsPrepend, append=gridAppend,
        model=graphicsGridModel1, filestem="graphics-grid-prepend-1",
        testCopy=FALSE, testVersion=FALSE)

# Test mix of 'graphics' and 'grid', with 'graphics' drawing already on device 
graphicsGrid2 <- function() {
    plot.new()
    require("grid", quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
}

graphicsGridModel2 <- function() {
    plot(1, col="green")
    require("grid", quietly=TRUE)
    plot.new()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.circle()    
    grid.edit("r", gp=gpar(col="red"))    
}

# Do not test dev.copy() for this one because 'prepend' does not make sense
# CANNOT test recorded plot from old R version because recordedplot
# will not contain grid DL, so append will not work
# (no "r" grob to find)
testAll(graphicsGrid2, prepend=graphicsPrepend, append=gridAppend,
        model=graphicsGridModel2, filestem="graphics-grid-prepend-2",
        testCopy=FALSE, testVersion=FALSE)

# Demonstration of the fact that the 'grid' DL is NOT erased when
# 'graphics' starts a new page, SO the 'grid' DL is included in
# the "recordedplot" and restored on replay (so 'grid' is involved
# in the replay because it thinks that there is 'grid' output on
# the engine DL [even though there is not];  a symptom of this
# involvement could be the injection of an unnecessary [blank] new page!)
gridDLgraphics <- function() {
    require("grid", quietly=TRUE)
    grid.segments()
    plot(1, col="red")
}

gridDLgraphicsModel <- function() {
    require("grid", quietly=TRUE)
    grid.segments()
    plot(1, col="red")
    plot(1, col="red")
}

testDevice(gridDLgraphics, model=gridDLgraphicsModel,
           filestem="grid-DL-graphics")

# This demonstration gets weirder;  we modify a grob on the 'grid' DL
# that is NOT visible on screen!
# Record/replay does work though (!)
gridDLgraphicsEdit <- function() {
    require("grid", quietly=TRUE)
    grid.segments(name="s")
    plot(1, col="red")
    grid.edit("s", gp=gpar(col="red"))
}

gridDLgraphicsEditModel <- function() {
    require("grid", quietly=TRUE)
    grid.segments(name="s")
    plot(1, col="red")
    grid.edit("s", gp=gpar(col="red"))
    # Replay of display list will force a new page
    grid.newpage()
    grid.segments(gp=gpar(col="red"))
}

testDevice(gridDLgraphicsEdit, model=gridDLgraphicsEditModel,
           filestem="grid-DL-graphics-edit")

# Tests for recording/replaying arbitrary R code on DL (recordGraphics)
# (pretty much any 'grid' code will do this anyway?)
recordGraphics <- function() {
    recordGraphics(plot(datasets::mtcars), list(), getNamespace("graphics"))
}

testAll(recordGraphics, model=recordGraphics, filestem="record-graphics")

# Tests for recording/replaying when there is NO 'grid' output
# CANNOT test recorded plot from old R version because recordedplot
# will not contain grid DL, so replay will not do ANY 'grid' setup
# (not even start a page)
gridNULL <- function() {
    require("grid", quietly=TRUE)    
    grid.newpage()
}

testAll(gridNULL, model=gridNULL, filestem="grid-null",
        testVersion=FALSE)

# Tests for copying BY REFERENCE
# (take copy of grid DL, modify grid DL, replay grid DL ...
#  ... get changes in replay?! [which would be bad!] )
# NO problem (because grid.edit() creates new object in old DL ?)
gridEdit <- function() {
    grid.edit("r", gp=gpar(col="red"))
}

gridEditModel <- function() {
    require("grid", quietly=TRUE)
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")
    grid.edit("r", gp=gpar(col="red"))
    # Replay of display list will force a new page
    grid.newpage()
    pushViewport(viewport(width=.5, height=.5, name="vp"))
    grid.rect(name="r")    
}

testDevice(gridPlot, prepend=gridEdit, model=gridEditModel,
           filestem="grid-edit")

# Test of local 'ggplot2' mod that records library(ggplot2) via
# recordGraphics() as part of the print.ggplot() method
# (so 'ggplot2' is automatically loaded if a 'ggplot2' plot
#  is on the recorded display list)
# FIRST test for existence of local 'ggplot2'
# (so this test just gets skipped if anyone else runs this code)
if (file.exists("../GGPlot")) {    
    # MAKE SURE that the local modded 'ggplot2' is installed in BOTH
    # 'Rcmd' R version and system R version (for testVersion=TRUE) !
    cmd <- paste0(Rcmd,
                  " -e 'install.packages(\"../GGPlot/ggplot2\", ",
                  "repos=NULL, ",
                  "lib=\"", Rlib, "\")'")
    system(cmd)
    system(paste0("Rscript -e 'install.packages(\"../GGPlot/ggplot2\", ",
                  "repos=NULL)'"))
    # Do the testing
    testAll(ggplot2Plot, model=ggplot2Plot, filestem="ggplot2-mod")
    # Restore normal ggplot2 installation
    cmd <- paste0(Rcmd,
                  " -e 'install.packages(\"ggplot2\", ",
                  "repos=\"http://cran.stat.auckland.ac.nz\", ",
                  "lib=\"", Rlib, "\")'")
    system(cmd)
    system(paste0("Rscript -e 'install.packages(\"ggplot2\", ",
                  "repos=\"http://cran.stat.auckland.ac.nz\")'"))
}
