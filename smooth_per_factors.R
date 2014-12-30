# Copyright (C) 2010 Tai Sakuma <sakuma@bnl.gov>
smooth.per.factors <- function(data, newx, xy.names, w.name, ...)
  {
    main <- function(data, degrees.of.freedom)
      {
        factor.names <- names(data)[!names(data) %in% c(xy.names, w.name)]
        if(length(factor.names) == 0)
          {
            data$ffffff <- 'f'
            factor.names <- c('ffffff')
          }
        b <- do.smooting.by.factors(data, factor.names, xy.names, w.name, ...)
        f <- smooting.by.to.data.frame(b, data, newx, factor.names, xy.names)
        h <- merge(data[factor.names], f)
        h[is.na(h[xy.names[2]]), xy.names[2]] <- 0
        f$ffffff <- NULL
        f
      }

    do.smooting.by.factors <- function(data, factor.names, xy.names, w.name, ...)
      {
        factor.list <- lapply(seq_along(factor.names), function(i) data[ ,factor.names[i]])
        names(factor.list) <- factor.names
        b <- by(data[c(xy.names, w.name)], factor.list, function(x) call.smooth.spline(x[ ,xy.names[1]], x[ ,xy.names[2]], x[ ,w.name], ...))
      }

    call.smooth.spline <- function(x, y, w, ...)
      {
        if(length(unique(x)) < 4) return(NULL) # smooth.spline() needs at least four points.
        s <- smooth.spline(x, y, w, ...)
        s
      }

    smooting.by.to.data.frame <- function(by, data, newx, factor.names, xy.names)
      {
        add.rows.smooth.spline <- function(blank.row, spline, newx)
          {
            if(is.null(spline)) { return(data.frame()) }
            if(length(factor.names) != 1)
                r <- blank.row[rep(1, length(newx)),]
            else
              {
                r <- data.frame(rep(blank.row, length(newx)))
                names(r) <- factor.names
              }
            pre <- predict(spline, newx)
            r <- cbind(r, data.frame(x = pre$x, y = pre$y))
            r
          }
        if(length(factor.names) == 0) return(NULL)
        factors <- lapply(factor.names, function(x) if(is.factor(data[[x]])) levels(data[[x]]) else levels(factor(data[[x]]) ))
        blank.data.frame <- expand.grid(factors)
        names(blank.data.frame) <- factor.names
        f <- by.to.data.frame(by, blank.data.frame, newx, add.rows.smooth.spline)
        f[xy.names[1]] = f$x
        f[xy.names[2]] = f$y
        if(!('x' %in% xy.names)) f$x <- NULL
        if(!('y' %in% xy.names)) f$y <- NULL
        f
      }

    by.to.data.frame <- function (by, blank.data.frame, newx, add.rows) 
      {
        dff <- data.frame()
        for(i in 1:nrow(blank.data.frame))
          {
            dff <- rbind(dff, add.rows(blank.data.frame[i,], by[[i]], newx))
          }
        dff
      }

    main(data, degrees.of.freedom)
  }

