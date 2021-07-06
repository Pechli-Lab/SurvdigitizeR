#' fun_idplot
#' Identifies plot location
#' @param fig.hsl array of pixels in HSL format (hue/saturation/lightness)
#' @param i.sen a double between 0 and 1 indicating sensitivity (What doesn't count as white space) the higher the value the more aggressive in ignoring colours similar to white.
#'
#' @return a list w/ 3 objects, fig.hsl: the array of pixels cropped to the axes, xaxis: a vector of integers to index plot by columns, yaxis: a vector of integers to index plot by rows.
#' @export
#'
#' @examples # fun_idplot(ls.fig = fun_readout, i.sen = 0.10)
fun_idplot  <- function(fig.hsl, i.sen = 0.05){
# To do
  # - make sure locations of zeroes doesnt break
  # - test w/ other curves
  # - possible for curve to be boxed in. create statement for that (ie v.rowsums)


# getting the lightness value
fig.l  <- fig.hsl[,,3]
# white is equal to 1 seeing how many have values are higher than sensitivitys
v.colsums <- colSums(1-fig.l > i.sen)
# idenftifying rowSums number of pixels where 1-pixel > i.sen
v.rowsums <- rowSums(1-fig.l > i.sen)

# identify all the collumns w/ rowsums close to our max
v.xaxis <-  which(v.colsums[which.max(v.colsums)]*0.95 < v.colsums)

# Possible for curve to be bounded in a box, checking if more than 1 x-axis
if(any(diff(v.xaxis) > dim(fig.l)[2]*0.4)){
  # Two xaxis
  x.br <- which(diff(v.xaxis) > dim(fig.l)[1]*0.3)
  x1 <- v.xaxis[1:x.br]
  x2 <- v.xaxis[-c(1:x.br)]

  xaxis <-  (max(x1) + 1):(min(x2)-1)

} else {

  i.xaxis <- max(v.xaxis)
  lc_zeroes_x <- which(v.colsums == 0 & lag(v.colsums, default = 0) == 0)
  suppressWarnings(i.xaxis.end <- min(lc_zeroes_x[(lc_zeroes_x > i.xaxis)]))
  if(is.finite(i.xaxis.end)){
    xaxis <- (i.xaxis+1):(i.xaxis.end-1)
  } else {
    xaxis <- (i.xaxis+1):length(v.colsums)
  }
}


# identify all the rows w/ rowsums close to our max
v.yaxis <-  which(v.rowsums[which.max(v.rowsums)]*0.95 < v.rowsums)

# possible for curves to be bounded in a box checking for more than 1 y-axis
if(any(diff(v.yaxis) > dim(fig.l)[1]*0.4)){
  # Two xaxis
  y.br <- which(diff(v.yaxis) > dim(fig.l)[1]*0.3)
  y1 <- v.yaxis[1:y.br]
  y2 <- v.yaxis[-c(1:y.br)]
  yaxis <-  (max(y1) + 1):(min(y2)-1)

} else {
  i.yaxis <- max(v.yaxis)
  # y axis starts at i.yaxis+1 and xaxis starts att i.xaxis +1
  # have the issue that might have a lot of 0's at the end want to crop them out
  lc_zeroes_y <- which(v.rowsums == 0 & lag(v.rowsums, default = 0) == 0)
  suppressWarnings(i.yaxis.end<- min(lc_zeroes_y[(lc_zeroes_y > i.yaxis)]))
if(is.finite(i.yaxis.end)){
      yaxis <- (i.yaxis+1):(i.yaxis.end-1)
  } else{
      yaxis <- (i.yaxis+1):length(v.rowsums)
  }
}

return(list(fig.hsl = fig.hsl[yaxis,xaxis,], axis=list(xaxis = xaxis, yaxis = yaxis)))
}

