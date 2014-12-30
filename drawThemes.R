# Copyright (C) 2011 Tai Sakuma <sakuma@fnal.gov>

##____________________________________________________________________________||
theme.size <- function()
  {
    list(
         fontsize = list(text = 9),
         par.main.text = list(cex = 1.0, font = 1),
         par.xlab.text = list(cex = 1),
         par.ylab.text = list(cex = 1),
         axis.text = list(cex = 1.0),
         add.text = list(cex = 1.0, lineheight = 2.0),
         axis.components = list(
           right = list(tck = 0.3, pad1 = 0.5, pad2 = 0),
           left = list(tck = 0.3, pad1 = 0.5, pad2 = 0),
           bottom = list(tck = 0.3, pad1 = 0.5, pad2 = 0.5),
           top = list(tck = 0, pad1 = 0, pad2 = 0))
         )
  }
##____________________________________________________________________________||
theme.set1 <- function()
  {
    cols = c("#00526D", "#00A3DB", "#7A2713", "#939598", "#6CCFF6")
    theme <- list(
         strip.border = list(lwd = 0.5),
         axis.line = list(lwd = 0.5),
         strip.background = list(col = c('gray85', 'gray85')),
         plot.symbol = list(col = cols[1], pch = 20, alpha = 0.9),
         plot.line = list(col = cols[1], alpha = 0.9),
         plot.polygon = list(col = cols[1], border = cols, lwd = 0, alpha = 0.9),
         superpose.polygon = list(col = cols, border = cols, lwd = 0),
         superpose.symbol = list(pch = c(20, 17), col = cols, cex = c(0.8, 0.7), alpha = 0.9),
         superpose.line = list(col = cols, lwd = c(0.8, 1.0, 1.0, 1.0, 1.0), alpha = 0.9),
         background = list(col = 'white')
         )
    modifyList(theme, theme.size())
  }
##____________________________________________________________________________||
theme.set2 <- function()
  {
    cols = c("#00526D", "#00A3DB", "#7A2713", "#939598", "#6CCFF6")
    cols = cols[c(4, 3, 5, 1, 2)]
    theme <- list(
         strip.border = list(lwd = 0.5),
         axis.line = list(lwd = 0.5),
         strip.background = list(col = c('gray85', 'gray85')),
         plot.symbol = list(col = cols[1], pch = 20, alpha = 0.9),
         plot.polygon = list(col = cols[1], border = cols, lwd = 0, alpha = 0.9),
         superpose.polygon = list(col = cols, border = cols, lwd = 0),
         superpose.symbol = list(pch = c(15, 18), col = cols, cex = c(1.0, 1.0), alpha = 0.9),
         superpose.line = list(col = cols, lwd = c(0.5, 1.0), alpha = 0.9),
         background = list(col = 'white')
         )
    modifyList(theme, theme.size())
  }
##____________________________________________________________________________||
theme.economist <- function()
  {
    cols = c("#00526D", "#00A3DB", "#7A2713", "#939598", "#6CCFF6")
    theme <- list(
         strip.border = list(lwd = 0),
         axis.line = list(lwd = 0),
         strip.background = list(col = 'gray85'),
         plot.symbol = list(col = cols[1], pch = 20),
         plot.line = list(col = cols[1], alpha = 0.9),
         barchart = list(col = cols[1]),
         plot.polygon = list(col = cols[1], border = cols, lwd = 0, alpha = 0.9),
         superpose.polygon = list(col = cols, border = cols, lwd = 0, alpha = 0.9),
         superpose.symbol = list(pch = c(20, 17), cex = c(1.2, 0.8), col = cols, alpha = 0.9),
         superpose.line = list(col = cols, lwd = c(0.8, 1.5), alpha = 0.9),
         reference.line = list(col = 'gray80'),
         background = list(col = 'white')
         )
    modifyList(theme, theme.size())
  }
##____________________________________________________________________________||
theme.economist2 <- function()
  {
    cols <- c('#d95f02', '#1b9e77')
    cols = c("#00A3DB", "#7A2713", "#939598", "#6CCFF6", "#00526D")
    theme <- list(
         strip.border = list(lwd = 0),
         axis.line = list(lwd = 0),
         strip.background = list(col = c('gray85', 'gray85')),
         plot.symbol = list(col = cols[1], pch = 20),
         plot.line = list(col = cols[1], alpha = 0.9),
         plot.polygon = list(col = cols[1], border = cols, lwd = 0, alpha = 0.9),
         superpose.polygon = list(col = cols, border = cols, lwd = 0, alpha = 0.9),
         superpose.symbol = list(pch = c(15, 18), col = cols, cex = c(1.0, 1.0), alpha = 0.9),
         superpose.line = list(col = cols, lwd = c(1.5, 0.8), alpha = 0.9),
         reference.line = list(col = 'gray80'),
         background = list(col = 'white')
         )
    modifyList(theme, theme.size())
  }
##____________________________________________________________________________||
theme.set3 <- function()
  {
    cols <- rev(rainbow_hcl(3, c = 90, l = 40))
    list(
         fontsize = list(text = 9),
         par.main.text = list(cex = 1, font = 1),
         axis.text = list(cex = 1),
         add.text = list(cex = 0.9, lineheight = 2.0),
         par.xlab.text = list(cex = 1),
         par.ylab.text = list(cex = 1),
         strip.border = list(lwd = 1),
         strip.background = list(col = c('gray85', 'gray85')),
         plot.symbol = list(col = c('black'), pch = 20),
         plot.line = list(col = c('black'), pch = 20),
         superpose.polygon = list(col = cols, border = cols, lwd = 0),
         superpose.symbol = list(pch = c(20, 17), col = cols),
         superpose.line = list(lty = 1:6, col = rep('gray5', 6), lwd = rep(1, 6)),
         axis.components = list(
           right = list(tck = 0.3, pad1 = 0.5, pad2 = 0),
           left = list(tck = 0.3, pad1 = 0.5, pad2 = 0),
           bottom = list(tck = 0.3, pad1 = 0.5, pad2 = 0.5),
           top = list(tck = 0.3, pad1 = 0.5, pad2 = 0.5)),
         reference.line = list(lwd = 0.5),
         background = list(col = 'white')
         )
  }

##____________________________________________________________________________||
theme.white.on.black <- function()
  {
    l <- trellis.par.get()
    for(n in names(l))
      for(nn in names(l[[n]]))
        if(nn == 'col')
          if(any(l[[n]][[nn]] == '#000000'))
    {
      l[[n]][[nn]][which(l[[n]][[nn]] == '#000000')] <- 'white'
      print(l[[n]][[nn]][which(l[[n]][[nn]] == '#000000')])
    }
    l$background$col = 'black'
    l
  }

##____________________________________________________________________________||
theme.xsec <- function()
  {
    cols <- c('gray5', 'gray20')
    theme <- list(
         strip.border = list(lwd = 0.5),
         axis.line = list(lwd = 0.5),
         strip.background = list(col = c('gray85', 'gray85')),
         plot.symbol = list(col = c('black'), pch = 20),
         superpose.polygon = list(col = cols, border = cols, lwd = 0),
         superpose.symbol = list(pch = c(20, 17), col = cols, cex = c(0.8, 0.7)),
         superpose.line = list(col = cols, lwd = c(0.5, 1.0), alpha = 0.9),
         background = list(col = 'white')
         )
    modifyList(theme, theme.size())
  }

##____________________________________________________________________________||
theme.time.series.1 <- function()
  {
    list(
         fontsize = list(text = 9),
         axis.text = list(cex = 1.0),
         add.text = list(cex = 1.0, lineheight = 2.0),
         par.xlab.text = list(cex = 1.0),
         par.ylab.text = list(cex = 1.0),
         par.main.text = list(cex = 1.0, font = 1),
         strip.background = list(col = c('gray85', 'gray85')),
         plot.symbol = list(col = c('black'), pch = 20),
         superpose.symbol = list(pch = 15:18, col = brewer.pal(8, 'Dark2'), cex = 0.8),
         superpose.line = list(col = brewer.pal(10, 'Set3')),
         axis.components = list(
           right = list(tck = 0.3, pad1 = 0.5, pad2 = 0),
           left = list(tck = 0.3, pad1 = 0.5, pad2 = 0),
           bottom = list(tck = 0.3, pad1 = 0.5, pad2 = 0.5),
           top = list(tck = 0, pad1 = 0, pad2 = 0)),
         background = list(col = 'white')
         )
  }

##____________________________________________________________________________||
theme.systematic.uncertainty <- function()
  {
    cols <- rev(rainbow_hcl(5, c = 90, l = 40))
    cols = c("#00526D", "#00A3DB", "#7A2713", "#939598", "#6CCFF6")
    cols <- rep(cols, each = 2)
    theme <- list(
         par.main.text = list(cex = 1),
         strip.border = list(lwd = 0.5),
         axis.line = list(lwd = 0.5),
         strip.background = list(col = c('gray85', 'gray85')),
         plot.symbol = list(col = c('black'), pch = 20),
         superpose.polygon = list(col = cols, border = cols, lwd = 0),
         superpose.symbol = list(pch = c(20, 17), col = cols, cex = c(0.8, 0.7)),
         superpose.line = list(col = cols, lwd = 1, alpha = 0.9, lty = rep(c(1, 2), each = 2)),
         background = list(col = 'white')
         )
    modifyList(theme, theme.size())
  }
##____________________________________________________________________________||
