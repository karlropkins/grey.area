############################################
#' @title tracePlotOnImage
############################################

#' @name tracePlotOnImage
#' @description Written for use in Muchammad Nafiek MSc project,
#' \code{tracePlotOnImage} allows you to trace information in a
#' image file.
#' @param n (integer) the maximum number of data points to collect
#' when tracing information on a plotted image. The option
#' \code{n = -1} allows open ended data collection.
#' @description   \code{tracePlotOnImage} allows the user to trace
#' information on an image, e.g. a picture file containing a bar graph
#' or plot, using the mouse left button. The procedure has three
#' steps: (1) The user is asked to select the the bottom corner of the
#' plot and enter its coordinates; (2) The user is asked to select the
#' top corner of the plot and enter its coordinates; and (3) The user
#' is asked to identify points of interest on the plot. The user can
#' select \code{n} points or stop the point collection process early
#' by pressing the mouse right button and selecting exit. The data
#' points, calibrated using information from steps (1) and (2), are
#' then returned.
#' @author Karl Ropkins (with contributions from Muchammad Nafiek)

#example currently not documented in package
##create e.g. png image of graph, e.g. img1.png
##in R
## #import and plot in R
## library(imager)
## im <- load.image(file.choose())
## plot(im)
## #run tracePlotOnImage and
## #follow instructions
## a <- tracePlotOnImage()
## #your traced plot data
## a

#splatted function
#' @export
tracePlotOnImage <-
function(n = 1000){

    #data tracer
    #karl

    message("select first ref (e.g. bottom left)")
    flush.console()

    ref.1 <- locator(1, type="p", col="red")
    ans <- readline("enter x,y coordinates: ")
    ref.x1 <- as.numeric(strsplit(ans, ",")[[1]][1])
    ref.y1 <- as.numeric(strsplit(ans, ",")[[1]][2])

    message("select second ref (e.g. top right)")
    flush.console()
    ref.2 <- locator(1, type="p", col="red")
    ans <- readline("enter x,y coordinates: ")
    ref.x2 <- as.numeric(strsplit(ans, ",")[[1]][1])
    ref.y2 <- as.numeric(strsplit(ans, ",")[[1]][2])

    y <- c(ref.y1, ref.y2)
    x <- c(ref.1$y, ref.2$y)
    mody <- lm(y~x)

    y <- c(ref.x1, ref.x2)
    x <- c(ref.1$x, ref.2$x)
    modx <- lm(y~x)

    message("select data then right click and exit")
    flush.console()
    actual <- locator(n, type="p", col="blue")

    actual$y<- predict(mody, newdata=data.frame(x=actual$y))
    actual$x<- predict(modx, newdata=data.frame(x=actual$x))

    actual
}
