# Tai Sakuma <sakuma@cern.ch>

##__________________________________________________________________||
## How to use this script.
##
## This script is designed to be used in a script that produces
## lattice plots
##
## 1: parse this script
## readArgs <- parse('drawReadArgs.R')
##
## 2: evaluate readArgs
## eval(readArgs)
##

##__________________________________________________________________||
## set default theme
theme.default <- if(exists('theme.default')) theme.default else 'theme.xsec'

##__________________________________________________________________||
## set default args
opts_default <- list(
             list(option = 'arg.outdir', default = 'fig'),
             list(option = 'arg.pdf', default = TRUE),
             list(option = 'arg.png', default = TRUE),
             list(option = 'arg.id',    default = NULL),
             list(option = 'arg.title',    default = TRUE),
             list(option = 'arg.transparent', default = FALSE)
             )


options = sapply(opts_default, function(x) x[['option']])
## i.e., c("arg.outdir", "arg.pdf", "arg.png", "arg.id", "arg.title", "arg.transparent")


##__________________________________________________________________||
## read command line args
com.args <- commandArgs(trailingOnly = TRUE)

## print them on the screen
cat('Args:', paste(com.args, sep = '', collapse = " "), '\n')

##__________________________________________________________________||
## assign options to variables e.g., "arg.outdir=\"fig_01\""
for(arg in com.args)
  {
    ## e.g., arg = "outdir=\"fig_01\""

    option <- strsplit(arg, split = "=")[[1]][1] # e.g., "outdir"
    option <- paste('arg.', option, sep = '') # e.g., "arg.outdir"

    if(!option %in% options) options <- c(options, option)

    ## parse and evaluate a string like "arg.outdir=\"fig_01\""
    ## which will actually assigns the value to the variable
    eval(parse(text = paste('arg.', arg, sep = '')))
  }

##__________________________________________________________________||
## assign default values if not assigned above
for(opt in opts_default)
  {
    cmd <- paste(opt$option, ' <- if(exists("', opt$option, '")) ', opt$option, ' else opt$default', sep = '')
    eval(parse(text = cmd))
  }

##__________________________________________________________________||
show.options <- function()
  {
    cat('\n')
    cat('Options:\n')
    for(opt in options)
      {
        value = eval(parse(text = opt))
        cat('\t', opt, ': ', value, '\n', sep = '')
      }
  }
## show.options()

##__________________________________________________________________||
