# Copyright (C) 2011 Tai Sakuma <sakuma@fnal.gov>

library('Hmisc', warn.conflicts = FALSE, quietly = TRUE)

##____________________________________________________________________________||
readArgs <- parse('latexReadArgs.R')

##____________________________________________________________________________||
mk.tex.id <- function(base = NULL, sub = NULL)
  {
    mk.tex.id.base <- function()
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
    
    base <- if(is.null(base)) mk.tex.id.base() else base
    tex.id <- if(is.null(sub)) base else paste(base, sub, sep = '_')
    tex.id <- if(is.null(arg.id)) tex.id else paste(tex.id, arg.id, sep = '_')
    tex.id
  }

##____________________________________________________________________________||
tex.file.name <- function(tex.id) paste(arg.outdir, '/', tex.id, '.tex', sep = '')

##____________________________________________________________________________||
pdf.file.name <- function(tex.id) paste(arg.outdir, '/', tex.id, '.pdf', sep = '')

##____________________________________________________________________________||
png.file.name <- function(tex.id) paste(arg.outdir, '/', tex.id, '.png', sep = '')

##____________________________________________________________________________||
mk.tex.pdf.file <- function(d, tex.id, mk.tex.file, caption = NULL, dvi.width = 5.5, dvi.height = 7, dvipdfm.options = '-p letter', ...)
  {
    l <- mk.tex.file(d, caption = caption, file = tex.file.name(tex.id), table.env = TRUE, ...)
    ## dvipdf.dvi(dvi.latex(l, nomargins = TRUE, width = dvi.width, height = dvi.height), pdf.file.name(tex.id), options = dvipdfm.options)
    pdf.latex(l, nomargins = TRUE, width = dvi.width, height = dvi.height, pdffilename = pdf.file.name(tex.id))
    l <- mk.tex.file(d, file = tex.file.name(tex.id), table.env = FALSE, ...)
    pdf.to.png(pdf.file.name(tex.id), png.file.name(tex.id))
  }
##____________________________________________________________________________||
pdf.latex <- function (object, prlog = FALSE, nomargins = FALSE, width = 5.5, height = 7, pdffilename, ...) 
{
    fi <- object$file
    sty <- object$style
    if (length(sty)) 
        sty <- paste("\\usepackage{", sty, "}", sep = "")
    if (nomargins) 
        sty <- c(sty, paste("\\usepackage[paperwidth=", width, 
            "in,paperheight=", height, "in,noheadfoot,margin=0in]{geometry}", 
            sep = ""))
    tmp <- tempfile()
    tmptex <- paste(tmp, "tex", sep = ".")
    infi <- readLines(fi, n = -1)
    # cat("\\documentclass[12pt]{report}", sty, "\\begin{document}\\pagestyle{empty}\\begin{table}[p]", 
    #    infi, "\\end{table}\\end{document}\n", file = tmptex, sep = "\n")
    cat("\\documentclass[12pt]{report}\\usepackage{amsmath}\\usepackage{kpfonts}\\usepackage[T1]{fontenc}", sty, "\\begin{document}\\pagestyle{empty}", 
        infi, "\\end{document}\n", file = tmptex, sep = "\n")
    sc <- "&&"
    sys(paste("cd", shQuote(tempdir()), sc, optionsCmds("pdflatex"), 
        "-interaction=scrollmode", shQuote(tmp)), output = FALSE)
    sys(paste("mv -f ", tmp, '.pdf ', pdffilename, sep = ''))
    if (prlog) 
        cat(scan(paste(tmp, "log", sep = "."), list(""), sep = "\n")[[1]], 
            sep = "\n")
    cat('written on', pdffilename, '\n')
}
##____________________________________________________________________________||
dvi.latex <- function (object, prlog = FALSE, nomargins = FALSE, width = 5.5, height = 7, ...) 
{
    fi <- object$file
    sty <- object$style
    if (length(sty)) 
        sty <- paste("\\usepackage{", sty, "}", sep = "")
    if (nomargins) 
        sty <- c(sty, paste("\\usepackage[paperwidth=", width, 
            "in,paperheight=", height, "in,noheadfoot,margin=0in]{geometry}", 
            sep = ""))
    tmp <- tempfile()
    tmptex <- paste(tmp, "tex", sep = ".")
    infi <- readLines(fi, n = -1)
    # cat("\\documentclass[12pt]{report}", sty, "\\begin{document}\\pagestyle{empty}\\begin{table}[p]", 
    #    infi, "\\end{table}\\end{document}\n", file = tmptex, sep = "\n")
    cat("\\documentclass[12pt]{report}\\usepackage{amsmath}\\usepackage{kpfonts}\\usepackage[T1]{fontenc}", sty, "\\begin{document}\\pagestyle{empty}", 
        infi, "\\end{document}\n", file = tmptex, sep = "\n")
    sc <- if (under.unix) {
        "&&"
    }
    else {
        "&"
    }
    sys(paste("cd", shQuote(tempdir()), sc, optionsCmds("latex"), 
        "-interaction=scrollmode", shQuote(tmp)), output = FALSE)
    if (prlog) 
        cat(scan(paste(tmp, "log", sep = "."), list(""), sep = "\n")[[1]], 
            sep = "\n")
    fi <- paste(tmp, "dvi", sep = ".")
    structure(list(file = fi), class = "dvi")
}
##____________________________________________________________________________||
dvipdf.dvi <- function (object, file, options = '-p letter', ...)
{
    cmd <- if (missing(file)) 
        paste(optionsCmds("dvipdfm"), options, " -m 1", shQuote(object$file))
    else paste(optionsCmds("dvipdfm"), options, " -m 1 -o", file, shQuote(object$file))
    invisible(sys(cmd))
}

##____________________________________________________________________________||
pdf.to.png <- function(pdf, png)
{
  ## cmd <- paste(optionsCmds("convert"), "-trim -density 120 -transparent \"#FFFFFF\"", shQuote(pdf), shQuote(png))
  cmd <- paste(optionsCmds("convert"), "-trim -density 110 -transparent \"#FFFFFF\"", shQuote(pdf), shQuote(png))
  invisible(sys(cmd))
}

##____________________________________________________________________________||
