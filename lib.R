# Tai Sakuma <sakuma@fnal.gov>

library('plyr', warn.conflicts = FALSE, quietly = TRUE)
library('reshape', warn.conflicts = FALSE, quietly = TRUE)

##____________________________________________________________________________||
call.write.table <- function(x, file)
  {
    cat('writing', file, '\n', file = stderr())
    write.table(format(x, digits = 4), file, quote = FALSE, row.names = FALSE)
  }

##____________________________________________________________________________||
call.write.table.aliened <- function(x, file, digits = 4)
  {
    cat('writing', file, '\n', file = stderr())
    for(col in names(x)) if(is.factor(x[[col]])) x[col] <- as.character(x[[col]])
    for(col in names(x)) if(is.integer(x[[col]])) x[col] <- as.character(x[[col]])
    for(col in names(x)) if(is.double(x[[col]])) x[col] <- sprintf(paste("%.", digits, "f", sep = ""), x[[col]])
    for(col in names(x)) if(is.character(x[[col]])) x[col] <- sprintf(paste("%", max(c(nchar(x[[col]]), nchar(col))), "s", sep = ""), x[[col]])
    for(col in names(x)) names(x)[names(x) == col] <- sprintf(paste("%", max(c(nchar(x[[col]]), nchar(col))), "s", sep = ""), col)
    write.table(x, file, quote = FALSE, row.names = FALSE)
  }

##____________________________________________________________________________||
create.blank.data.frame <- function(data, except = '')
  {
    factor.names <- names(data)[!names(data) %in% except]
    factors <- sapply(factor.names, function(x) if(is.factor(data[[x]])) levels(data[[x]]) else levels(factor(data[[x]]) ))
    expand.grid(factors)
  }

##____________________________________________________________________________||
apply.correction <- function(data, correction.factor)
  { 
    d <- merge(data, correction.factor)
    if('var.n' %in% names(d))
      {
        if('var.cor' %in% names(d))
          d$var.n <- (d$var.n/d$n^2 + d$var.cor/d$cor^2)*(d$n*d$cor)^2
        else
          d$var.n <- d$var.n*d$cor^2
      }
    d$n <- d$n*d$cor
    d$cor <- NULL
    d$var.cor <- NULL
    d
  }

##____________________________________________________________________________||
normalize.y.over.x.categories <- function(data, categories = '', x = 'x', y = 'y', var.y = 'var.y')
  { # normalize data$y over data$x per each category. use all other columns as categories.
    factor.names <- names(data)[!names(data) %in% c(categories, x, y, var.y)]
    factor.list <- lapply(seq_along(factor.names), function(i) data[,factor.names[i]])
    names(factor.list) <- factor.names
    a <- aggregate(data[y], factor.list, sum)
    a$nn <- a[, y]
    a[y] <- NULL
    data <- merge(data, a)
    data[, y] <- ifelse(data$nn == 0, 0, data[, y]/data$nn)
    if(var.y %in% names(data)) data[var.y] <- ifelse(data$nn == 0, 0, data[, var.y]/(data$nn^2))
    data$nn <- NULL
    data
  }

##____________________________________________________________________________||
normalize.y.over.x <- function(data, x = 'x', y = 'y', var.y = 'var.y', bin.min = NULL, bin.max = NULL)
  { # normalize data$y over data$x per each category. use all other columns as categories.
    d <- data
    if(!is.null(bin.min)) data <- data[data[, x] >= bin.min, ]
    if(!is.null(bin.max)) data <- data[data[, x] <= bin.max, ]
    factor.names <- names(data)[!names(data) %in% c(x, y, var.y)]
    factor.list <- lapply(seq_along(factor.names), function(i) data[,factor.names[i]])
    names(factor.list) <- factor.names
    a <- aggregate(data[y], factor.list, sum)
    a$nn <- a[, y]
    a[y] <- NULL
    data <- merge(d, a)
    data[, y] <- ifelse(data$nn == 0, 0, data[, y]/data$nn)
    if(var.y %in% names(data)) data[var.y] <- ifelse(data$nn == 0, 0, data[, var.y]/(data$nn^2))
    data$nn <- NULL
    data
  }

##____________________________________________________________________________||
normalize.n.over.zbin <- function(data, x = 'zbin', y = 'n', var.y = 'var.n', bin.min = NULL, bin.max = NULL)
  {
    normalize.y.over.x(data, x, y, var.y, bin.min, bin.max)
  }

##____________________________________________________________________________||
normalize.n.over.mbin <- function(data, x = 'mbin', y = 'n', var.y = 'var.n', bin.min = NULL, bin.max = NULL)
  {
    normalize.y.over.x(data, x, y, var.y, bin.min, bin.max)
  }

##____________________________________________________________________________||
normalize.n.over.ptbin <- function(data, x = 'ptbin', y = 'n', var.y = 'var.n', bin.min = NULL, bin.max = NULL)
  {
    normalize.y.over.x(data, x, y, var.y, bin.min, bin.max)
  }

##____________________________________________________________________________||
normalize.n.over.bin <- function(data, x = 'bin', y = 'n', var.y = 'var.n', bin.min = NULL, bin.max = NULL)
  {
    normalize.y.over.x(data, x, y, var.y, bin.min, bin.max)
  }

##____________________________________________________________________________||
sum.n.over.categories.variables.reshape <- function(data, categories, variables = c('n', 'var.n'))
  {
    factor.names <- names(data)[!names(data) %in% c(variables, categories)]
    data.m <- melt(data, id = c(factor.names, categories), measured = variables)
    formula <- paste(paste(factor.names, collapse = ' + '), ' ~ variable')
    data.r <- cast(data.m, formula, sum)
    data.frame(data.r)
  }

##____________________________________________________________________________||
sum.n.over.categories.reshape <- function(data, categories, n = 'n', var.n = 'var.n')
  {
    factor.names <- names(data)[!names(data) %in% c(n, var.n, categories)]
    variables <- c(n, var.n)[c(n, var.n) != '']
    data.m <- melt(data, id = c(factor.names, categories), measured = variables)
    formula <- paste(paste(factor.names, collapse = ' + '), ' ~ variable')
    data.r <- cast(data.m, formula, sum)
    data.frame(data.r)
  }

##____________________________________________________________________________||
sum.n.over.categories <- function(data, categories, n = 'n', var.n = 'var.n')
  {
    factor.names <- names(data)[!names(data) %in% c(n, var.n, categories)]
    factor.list <- lapply(seq_along(factor.names), function(i) data[,factor.names[i]])
    names(factor.list) <- factor.names
    
    f <- aggregate(data[n], factor.list, sum)
    names(f)[which(names(f) == 'x')] <- n

    if(var.n %in% names(data))
      {
        g <- aggregate(data[var.n], factor.list, sum)
        names(g)[which(names(g) == 'x')] <- var.n
        f <- merge(f, g)
      }
    f
  }

##____________________________________________________________________________||
rebin.bin <- function(data, bin, nbin = 2)
  {
    data[, bin] <- ceiling(data[, bin]/nbin)
    data <- sum.n.over.categories(data, '', 'n', 'var.n')
    data
  }

##____________________________________________________________________________||
take.ratio <- function(data1, data2)
  {
    data1$nd <- data1$n
    data2$nm <- data2$n
    data1$n <- NULL
    data2$n <- NULL

    do.var.n <- FALSE
    if('var.n' %in% names(data1) & 'var.n' %in% names(data2))
      {
        do.var.n <- TRUE
        data1$var.n.d <- data1$var.n
        data2$var.n.n <- data2$var.n
        data1$var.n <- NULL
        data2$var.n <- NULL
      }
    
    d <- merge(data1, data2)
    d$cor <- d$nd/d$nm

    if(do.var.n)
      d$var.cor <- (d$var.n.d/(d$nd^2) + d$var.n.n/(d$nm^2))*d$cor^2

    d$nm <- NULL
    d$nd <- NULL

    d$var.n.d <- NULL
    d$var.n.n <- NULL

    d
  }

##____________________________________________________________________________||
mbin.l <- function(mbin, breaks)
  {
    paste(breaks[mbin], ' < M < ', breaks[mbin + 1], ' GeV', sep = '')
  }

##____________________________________________________________________________||
ptbin.l <- function(bin, breaks)
  {
    paste(breaks[bin], ' < pT < ', breaks[bin + 1], ' GeV', sep = '')
  }

##____________________________________________________________________________||
ptbin.m <- function(bin, breaks)
  {
    paste(bin, breaks[bin], breaks[bin + 1], sep = ' ')
  }

##____________________________________________________________________________||
bin.c <- function(bin, breaks)
  {
    (breaks[bin] + breaks[bin + 1])*0.5
  }

##____________________________________________________________________________||
