#!/usr/bin/env Rscript
# Tai Sakuma <sakuma@fnal.gov>

##____________________________________________________________________________||
main <- function(inputFile, outputFile)
  {
    d <- read.table(inputFile, header = TRUE, colClasses = 'character')
    for(col in names(d)) d[col] <- sprintf(paste("%", max(c(nchar(d[[col]]), nchar(col))), "s", sep = ""), d[[col]])
    for(col in names(d)) names(d)[names(d) == col] <- sprintf(paste("%", max(c(nchar(d[[col]]), nchar(col))), "s", sep = ""), col)
    cat('writing', outputFile, '\n')
    write.table(d, outputFile, quote = FALSE, row.names = FALSE)
  }
##____________________________________________________________________________||

com.args <- commandArgs(trailingOnly = TRUE)
if(length(com.args) != 2) q()
inputFile <- com.args[1]
outputFile <- com.args[2]
main(inputFile, outputFile)
