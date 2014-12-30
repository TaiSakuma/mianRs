# Copyright (C) 2011 Tai Sakuma <sakuma@fnal.gov>

##____________________________________________________________________________||
opts <- list(
             list(option = 'arg.outdir', default = 'ltx'),
             list(option = 'arg.id',    default = NULL)
             )

##____________________________________________________________________________||
com.args <- commandArgs(trailingOnly = TRUE)
cat('Args:', paste(com.args, sep = '', collapse = " "), '\n')
for(arg in com.args) eval(parse(text = paste('arg.', com.args, sep = '')))

##____________________________________________________________________________||
for(opt in opts)
  {
    cmd <- paste(opt$option, ' <- if(exists("', opt$option, '")) ', opt$option, ' else opt$default', sep = '')
    eval(parse(text = cmd))
  }

##____________________________________________________________________________||
