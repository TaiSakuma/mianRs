#!/usr/bin/env Rscript
# Tai Sakuma <sakuma@fnal.gov>

library('plyr', warn.conflicts = FALSE, quietly = TRUE)
library('reshape', warn.conflicts = FALSE, quietly = TRUE)

##____________________________________________________________________________||
main <- function(inputFiles)
  {
    ret <- data.frame()
    for(file in inputFiles)
      {
        d <- read.table(file, header = TRUE)
        d$var.n <- d$n
        ret <- rbind(ret, d)
      }
    ret <- do.add(ret)
    write.table(ret, stdout(), quote = TRUE, row.names = FALSE)
    ## write.table(format(ret, digits = 4), stdout(), quote = FALSE, row.names = FALSE)
  }
##____________________________________________________________________________||
do.add <- function(data, n = 'n', var.n = 'var.n')
  {
    factor.names <- names(data)[!names(data) %in% c(n, var.n)]
    variables <- c(n, var.n)[c(n, var.n) != '']
    data.m <- melt(data, id = c(factor.names), measured = variables)
    formula <- paste(paste(factor.names, collapse = ' + '), ' ~ variable')
    data.r <- cast(data.m, formula, sum)
    data.frame(data.r)
  }

##____________________________________________________________________________||

com.args <- commandArgs(trailingOnly = TRUE)

main(inputFiles = com.args)
