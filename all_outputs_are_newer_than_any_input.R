#!/usr/bin/env Rscript
# Tai Sakuma <sakuma@cern.ch>

##__________________________________________________________________||
all_outputs_are_newer_than_any_input <- function(outPaths, inPaths)
  {
    existingInPaths <- inPaths[file.exists(inPaths)]
    if ( ! all(file.exists(outPaths)) ) return(FALSE)
    timeOutFiles <- file.info(outPaths)[ , "mtime"]
    timeInFiles <- file.info(existingInPaths)[ , "mtime"]
    return(all(outer(timeOutFiles, timeInFiles, difftime) > 0))
  }

##__________________________________________________________________||
