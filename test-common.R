
Rcmd <- "~/R/r-devel/BUILD/bin/Rscript"
Rlib <- "~/R/r-devel/BUILD/library"
    
cat(paste0("===================================================\n",
           "Path to Rscript executable (for testing) set to ...\n",
           Rcmd, "\n",
           "===================================================\n"))

funText <- function(f) {
    bodyText <- deparse(body(f))
    paste(bodyText[-c(1, length(bodyText))], collapse=";")
}

doNothing <- function() {
    NULL
}

makeModel <- function(model, filestem, suffix, dev="png") {
    png <- paste0(filestem, "-", suffix, "-model%02d.", dev)
    code <- funText(model)
    expr <- paste0(dev, '("', png, '"); dev.control("enable"); ',
                   code)
    cmd <- paste0(Rcmd,  " -e '", expr, "'")
    result <- system(cmd, ignore.stdout=TRUE, ignore.stderr=FALSE)
    if (result) {
        stop("Failed to generate model plot")
    }
}

compareResult <- function(filestem, suffix, dev="png") {
    # (may involve comparing multiple files)
    replayFiles <- list.files(pattern=paste0(filestem, "-", suffix,
                                  "-replay.*.", dev))
    modelFiles <- list.files(pattern=paste0(filestem, "-", suffix,
                                 "-model.*.", dev))
    if (length(replayFiles) != length(modelFiles)) {
        stop(paste0("Number of replay files (", length(replayFiles),
                    ") does not match number of model files (",
                    length(modelFiles), ")"))
    }
    for (i in seq_along(modelFiles)) {
        cmpfile <- paste0(filestem, "-diff.png")
        cmpResult <- system2("compare",
                             c("-metric ae",
                               replayFiles[i], modelFiles[i], cmpfile),
                             stdout=TRUE, stderr=TRUE)
        if (cmpResult != "0") {
            stop(paste0("Files ", replayFiles[i], " and ", modelFiles[i],
                        " do not match"))
        } else {
            cat(paste0(replayFiles[i], " = ", modelFiles[i], "\n"))
        }
    }
}

testCopy <- function(plot, append=doNothing, model, filestem,
                     dev="png") {
    # Create plot and copy to new device (possibly prepending and appending)
    png1 <- paste0(filestem, "-copy-", dev, "-record.", dev)
    png2 <- paste0(filestem, "-copy-", dev, "-replay%02d.", dev)
    code1 <- funText(plot)
    code2 <- funText(append)
    expr <- paste0(dev, '("', png1, '"); dev.control("enable"); ',
                   code1,
                   '; dev.copy(', dev, ', file="', png2, '"); ',
                   code2)
    cmd <- paste0(Rcmd,  " -e '", expr, "'")
    result <- system(cmd, ignore.stdout=TRUE, ignore.stderr=FALSE)
    if (result) {
        stop("Failed to copy recorded plot")
    }
    # Produce model answer for copied plot
    makeModel(model, filestem, paste0("copy-", dev), dev=dev)
    # Compare copied plot with model answer
    compareResult(filestem, paste0("copy-", dev), dev=dev)
}

testDevice <- function(plot, prepend=doNothing, append=doNothing, model,
                       filestem) {
    # Create plot and replay on SAME device (possibly prepending and appending)
    png <- paste0(filestem, "-device-replay%02d.png")
    code1 <- funText(plot)
    code2 <- funText(prepend)
    code3 <- funText(append)
    expr <- paste0('png("', png, '"); dev.control("enable"); ',
                    code1,
                    '; p <- recordPlot(); ',
                    code2,
                    '; replayPlot(p); ',
                    code3)
    cmd <- paste0(Rcmd,  " -e '", expr, "'")
    result <- system(cmd, ignore.stdout=TRUE, ignore.stderr=FALSE)
    if (result) {
        stop("Failed to replay recorded plot")
    }
    makeModel(model, filestem, "device")
    # Compare copied plot with model answer
    compareResult(filestem, "device")
}

testSession <- function(plot, prepend=doNothing, append=doNothing, model,
                       filestem) {
    # Create plot and replay IN SAME R SESSION
    # (possibly prepending and appending)
    png1 <- paste0(filestem, "-session-record%02d.png")
    png2 <- paste0(filestem, "-session-replay%02d.png")
    code1 <- funText(plot)
    code2 <- funText(prepend)
    code3 <- funText(append)
    expr <- paste0('png("', png1, '"); dev.control("enable"); ',
                   code1,
                   '; p <- recordPlot(); dev.off(); ',
                   'png("', png2, '"); ',
                    code2,
                    '; replayPlot(p); ',
                    code3)
    cmd <- paste0(Rcmd,  " -e '", expr, "'")
    result <- system(cmd, ignore.stdout=TRUE, ignore.stderr=FALSE)
    if (result) {
        stop("Failed to either record plot or replay recorded plot")
    }
    makeModel(model, filestem, "session")
    # Compare copied plot with model answer
    compareResult(filestem, "session")
}

testReload <- function(plot, prepend=doNothing, append=doNothing, model,
                       filestem, testVersion=FALSE, defaultPackages=NA,
                       loadPkgs=NULL, attachPkgs=NULL, reloadPkgs=FALSE) {
    # Record plot
    savefile <- paste0(filestem, "-reload-record.rds")
    png1 <- paste0(filestem, "-reload-record.png")
    code1 <- funText(plot)
    # If loadPkgs != NULL or attachPkgs != NULL record packages with snapshot
    if (is.null(loadPkgs)) {
        pkgs <- ""
    } else {
        pkgs <- paste0('load="', paste(loadPkgs, collapse='", "'), '"')
    }
    if (is.null(attachPkgs)) {
        pkgs <- pkgs
    } else {
        attach <- paste0('attach="', paste(attachPkgs, collapse='", "'), '"')
        if (is.null(loadPkgs)) {
            pkgs <- attach
        } else {
            pkgs <- paste(pkgs, attach, sep=", ")
        }
    }
    expr1 <- paste0('png("', png1, '"); dev.control("enable"); ',
                    code1,
                    '; p <- recordPlot(', pkgs, '); ',
                    'saveRDS(p, "', savefile, '")')
    # If 'testVersion', generate the "recordedplot" using the system R
    # (which will be a different R version)
    if (testVersion) {
        cmd1 <- paste0("Rscript -e '", expr1, "'")
    } else {
        cmd1 <- paste0(Rcmd,  " -e '", expr1, "'")
    }
    result <- system(cmd1, ignore.stdout=TRUE, ignore.stderr=FALSE)
    if (result) {
        stop("Failed to generate recorded plot")
    }
    # Replay plot (possibly prepend and possibly append new drawing)
    png2 <- paste0(filestem, "-reload-replay%02d.png")
    code2a <- funText(prepend)
    code2b <- funText(append)
    # If defaultPackages != NA, reload the "recordedplot" in R session
    # with only specified packages loaded
    if (is.null(defaultPackages)) {
        dfltpkgs <- " --default-packages=NULL"
    } else {
        if (length(defaultPackages) == 1 && is.na(defaultPackages)) {
            dfltpkgs <- ""
        } else {
            dfltpkgs <- paste0(" --default-packages=",
                               paste(defaultPackages, collapse=","))
        }
    }
    # Specify 'grDevices' in case NO default packages are loaded
    expr2 <- paste0('grDevices::png("', png2, '"); ',
                    'grDevices::dev.control("enable"); ',
                    code2a,
                    '; p <- readRDS("', savefile, '"); ',
                    # reloadPkgs if told to
                    'grDevices::replayPlot(p, reloadPkgs=', reloadPkgs, '); ',
                    code2b)
    cmd2 <- paste0(Rcmd,  dfltpkgs, " -e '", expr2, "'")
    result <- system(cmd2, ignore.stdout=TRUE, ignore.stderr=FALSE)
    if (result) {
        stop("Failed to reload recorded plot")
    }
    # Produce model answer for replayed plot
    makeModel(model, filestem, "reload")
    # Compare replayed plot with model answer
    compareResult(filestem, "reload")
}

# Compendium of tests
# Set testReload=FALSE to avoid test of recordedplot from different R session
# Set testCopy=FALSE to avoid test of dev.copy()
# Set testVersion=FALSE to avoid test of recordedplot from different R version
testAll <- function(plot, prepend=doNothing, append=doNothing, model, filestem,
                    testReload=TRUE, testCopy=TRUE, testVersion=TRUE,
                    dev=c("png", "postscript", "pdf", "svg")) {
    # Test replay of DL within same R session
    testSession(plot, prepend, append, model, filestem)
    if (testCopy) {
        # Test copy of DL to another device (for several different formats)
        # png() will exercise X11 device driver (on Linux)
        # postscript() will exercise PostScript device driver
        # pdf() will exercise PDF device driver
        # svg() will exercise Cairo device driver (on Linux)
        for (i in dev) {
            testCopy(plot, append, model, filestem, dev=i)
        }
    }
    if (testReload) {
        # Test replay of DL in different R session
        # (same R version)
        testReload(plot, prepend, append, model, filestem)
        # Test replay of DL in different R session
        # (same R version, no graphics packages)
        testReload(plot, prepend, append, model,
                   paste0(filestem, "-no-graphics"),
                   defaultPackages=NULL)
        # Test replay of DL in different R session
        # (same R version, graphics packages reversed)
        testReload(plot, prepend, append, model,
                   paste0(filestem, "-grid-first"),
                   defaultPackages=c("grid", "graphics"))
        if (testVersion) {
            # Test replay of DL in different R session
            # (different R version)
            testReload(plot, prepend, append, model,
                       paste0(filestem, "-R-version"),
                       testVersion=TRUE)
        }
    }
}
