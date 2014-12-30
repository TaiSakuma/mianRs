quartz.save <- function (file, type = "png", device = dev.cur(), dpi = 110, 
    ...) 
{
    dev.set(device)
    current.device <- dev.cur()
    nm <- names(current.device)[1]
    if (nm == "null device") 
        stop("no device to print from")
    oc <- match.call()
    oc[[1]] <- as.name("dev.copy")
    oc$file <- NULL
    oc$device <- quartz
    oc$type <- type
    oc$file <- file
    oc$dpi <- dpi
    din <- dev.size("in")
    w <- din[1]
    h <- din[2]
    if (is.null(oc$width)) 
        oc$width <- if (!is.null(oc$height)) 
            w/h * eval.parent(oc$height)
        else w
    if (is.null(oc$height)) 
        oc$height <- if (!is.null(oc$width)) 
            h/w * eval.parent(oc$width)
        else h
    dev.off(eval.parent(oc))
    dev.set(current.device)
}
