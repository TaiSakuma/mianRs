#!/usr/bin/env Rscript
# Tai Sakuma <sakuma@fnal.gov>

##____________________________________________________________________________||
main <- function(par.name, par.val)
  {
    d <- data.frame(par.val)
    names(d)[names(d) == 'par.val'] <- par.name
    write.table(format(d, digits = 4), stdout(), quote = FALSE, row.names = FALSE)
  }
##____________________________________________________________________________||

com.args <- commandArgs(trailingOnly = TRUE)
if(length(com.args) != 2) q()
par.name <- com.args[1]
par.val <- com.args[2]
main(par.name, par.val)
