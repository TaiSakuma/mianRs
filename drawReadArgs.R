# Tai Sakuma <sakuma@fnal.gov>

##____________________________________________________________________________||
theme.default <- if(exists('theme.default')) theme.default else 'theme.xsec'

##____________________________________________________________________________||
opts <- list(
             list(option = 'arg.outdir', default = 'fig'),
             list(option = 'arg.pdf', default = TRUE),
             list(option = 'arg.png', default = TRUE),
             list(option = 'arg.theme', default = theme.default),
             list(option = 'arg.id',    default = NULL),
             list(option = 'arg.title',    default = TRUE),
             list(option = 'arg.transparent', default = FALSE)
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
show.options <- function()
  {
    cat('\n')
    cat('Options:\n')
    for(opt in opts)
      {
        opt.value = eval(parse(text = opt$option))
        opt.value
        cat(sprintf('%4s:\n', substring(opt$option, 6)), sep = ' ')
        str(opt.value)
      }
  }
# show.options()

##____________________________________________________________________________||
if(length(dev.list()) == 0)  quartz(width = 4, height = 4)
trellis.par.set(theme = arg.theme)
if(arg.transparent) trellis.par.set(background = list(col = 'transparent'))
# show.settings()
                

##____________________________________________________________________________||
