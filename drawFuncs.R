# Tai Sakuma <sakuma@fnal.gov>

library('lattice', warn.conflicts = FALSE, quietly = TRUE)
library('latticeExtra', warn.conflicts = FALSE, quietly = TRUE)
library('grid', warn.conflicts = FALSE, quietly = TRUE)
library('RColorBrewer', warn.conflicts = FALSE, quietly = TRUE)
library('methods', warn.conflicts = FALSE, quietly = TRUE)
library('vcd', warn.conflicts = FALSE, quietly = TRUE)
library('colorspace', warn.conflicts = FALSE, quietly = TRUE)


##____________________________________________________________________________||
readArgs <- parse('drawReadArgs.R')

##____________________________________________________________________________||
mk.fig.id <- function(base = NULL, sub = NULL)
  {
    mk.fig.id.base <- function()
      {
        argv <- commandArgs(trailingOnly = FALSE)
        bn <- basename(substring(argv[grep("--file=", argv)], 8))
        fxxx <- sub('.R', '', bn)
        fxxx <- strsplit(fxxx, '_')[[1]][2]

        wd <- getwd()
        wd.bn <- basename(wd)
        sxxxx <- strsplit(wd.bn, '_')[[1]][1]

        wd.parent.bn <- basename(dirname(wd))
        cxxxxxx <- strsplit(wd.parent.bn, '_')[[1]][1]

        paste(cxxxxxx, sxxxx, fxxx, sep = '_')
      }
    
    base <- if(is.null(base)) mk.fig.id.base() else base
    fig.id <- if(is.null(sub)) base else paste(base, sub, sep = '_')
    fig.id <- if(is.null(arg.id)) fig.id else paste(fig.id, arg.id, sep = '_')
    fig.id
  }

##____________________________________________________________________________||
print.figure <- function(trellis, fig.id,
                         width = 4, height = 4,
                         theme = lattice.getOption("default.theme"))
  {
    filepath_noext <- file.path(arg.outdir, fig.id)
    if(arg.pdf)
      {
        filepath <- paste(filepath_noext, 'pdf', sep = '.')
        print(filepath)
        trellis.device(device = pdf, file = filepath, theme = theme, width = width, height = height)
        print(trellis)
        dev.off()
      }

    if(arg.png)
      {
        filepath <- paste(filepath_noext, 'png', sep = '.')
        print(filepath)
        trellis.device(device = png, file = filepath, theme = theme, width = width, height = height, units = 'in', res = 300)
        print(trellis)
        dev.off()
      }

  }
##____________________________________________________________________________||
log10.y.labels <- function(y.at)
  {
    # y.labels.cha <- ifelse(abs(y.at) >= 2, paste('10^{', y.at, '}', sep = ''), 10^y.at)
    y.labels.cha <- ifelse(y.at %% 1 == 0, ifelse(abs(y.at) >= 2, paste('10^{', y.at, '}', sep = ''), 10^y.at), '')
    sapply(y.labels.cha, function(x) if(nchar(x) == 0) x else parse(text = x))
  }

##____________________________________________________________________________||
log10.y.grid.at <- function(y.at)
  {
    y <- sort(as.vector(sapply(1:9*10, function(x) log10(x*10^{(min(y.at) - 1):(max(y.at) - 1)}))))
    y[y <= max(y.at)]
  }

##____________________________________________________________________________||
