#!/usr/bin/env Rscript
# Tai Sakuma <sakuma@fnal.gov>

##____________________________________________________________________________||
main <- function(in.base, parm.base, in.dirs)
  {
    d.out <- data.frame()
    for(in.dir in in.dirs)
      {
        in.path <- paste(in.dir, '/', in.base, sep = '')
        parm.path <- paste(in.dir, '/', parm.base, sep = '')
        d <- read.table(in.path, header = TRUE)
        parm <- read.table(parm.path, header = TRUE)
        d <- merge(d, parm)
        d.out <- rbind(d.out, d)
      }
    write.table(format(d.out, digits = 4), stdout(), quote = FALSE, row.names = FALSE)
  }
##____________________________________________________________________________||
  
com.args <- commandArgs(trailingOnly = TRUE)
if( ! length(com.args) >= 3) q()
in.base <- com.args[1]
parm.base <- com.args[2]
in.dirs <- com.args[3:length(com.args)]

main(in.base, parm.base, in.dirs)
