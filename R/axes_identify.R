#' axes_identify
#' Identifies plot axes location
#' @param fig.hsl array of pixels in HSL format (hue/saturation/lightness)
#' @param bg_lightness a lightness threshold value between 0 and 1; every pixel with lightness > bg_lightness is considered background and removed (default: 0.1).
#'
#' @return a list w/ 2 objects, fig.hsl: the array of pixels cropped to the axes, axes: a list of xaxis, yaxis: vectors of integers to index plot by columns and rows respectively.
#' @export
#'
#' @examples # axes_identify(fig.hsl = figure, bg_lightness = 0.1)
axes_identify  <- function(fig.hsl, bg_lightness = 0.1){
  # To do
  # - make sure locations of zeroes doesnt break
  # - test w/ other curves
  # - possible for curve to be boxed in. create statement for that (ie v.rowsums)

  multi_ind_x = -1
  multi_ind_y = -1

  step_1 = fig.hsl

  max_prop = 0.9
  bg_lightness  = 0.1
  # getting the lightness value
  fig.l  <- fig.hsl[,,3]
  # white is equal to 1 seeing how many have values are higher than bg_lightness
  v.colsums <- colSums(1-fig.l > bg_lightness)
  # identifying rowSums number of pixels where 1-pixel > bg_lightness
  v.rowsums <- rowSums(1-fig.l > bg_lightness)

  # identify all the collumns w/ rowsums close to our max
  v.xaxis <-  which(v.colsums[which.max(v.colsums)]*max_prop < v.colsums)

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
  v.yaxis <-  which(v.rowsums[which.max(v.rowsums)]*max_prop < v.rowsums)

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

  xaxis0.9 = xaxis
  yaxis0.9 = yaxis


  max_prop = 0.6
  # getting the lightness value
  fig.l  <- fig.hsl[,,3]
  # white is equal to 1 seeing how many have values are higher than bg_lightness
  v.colsums <- colSums(1-fig.l > bg_lightness)
  # identifying rowSums number of pixels where 1-pixel > bg_lightness
  v.rowsums <- rowSums(1-fig.l > bg_lightness)

  # identify all the collumns w/ rowsums close to our max
  v.xaxis <-  which(v.colsums[which.max(v.colsums)]*max_prop < v.colsums)

  # Possible for curve to be bounded in a box, checking if more than 1 x-axis
  if(any(diff(v.xaxis) > dim(fig.l)[2]*0.4)){
    # Two xaxis
    x.br <- which(diff(v.xaxis) > dim(fig.l)[1]*0.3)
    x1 <- v.xaxis[1:x.br]
    x2 <- v.xaxis[-c(1:x.br)]

    xaxis <-  (max(x1) + 1):(min(x2)-1)
    multi_ind_x = 1

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
  v.yaxis <-  which(v.rowsums[which.max(v.rowsums)]*max_prop < v.rowsums)

  # possible for curves to be bounded in a box checking for more than 1 y-axis
  if(any(diff(v.yaxis) > dim(fig.l)[1]*0.4)){
    # Two xaxis
    y.br <- which(diff(v.yaxis) > dim(fig.l)[1]*0.3)
    y1 <- v.yaxis[1:y.br]
    y2 <- v.yaxis[-c(1:y.br)]
    yaxis <-  (max(y1) + 1):(min(y2)-1)

    multi_ind_y = 1

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

  xaxis0.6 = xaxis
  yaxis0.6 = yaxis


  #check if there's a grey axis, if so, update the grey axis to black, convient for downstream detection steps
  if(abs(length(xaxis0.6) - length(xaxis0.9)) > 50){

    xaxis = xaxis0.6
    yaxis = yaxis0.6

    fig_l_new = fig.l

    if((multi_ind_y != -1) & (multi_ind_x != -1)){

      thick_x_axis = v.yaxis[(tail(which(diff(v.yaxis)[1:(y.br)-1] >1)) + 1):y.br]

      # white is equal to 1 seeing how many have values are higher than bg_lightness
      v.colsums_xaxis <- colSums(1-fig.l[thick_x_axis,] > bg_lightness)
      x_axis_end = tail(which(diff(v.colsums_xaxis) != 0),2)[1]


      thick_y_axis = v.xaxis[(tail(which(diff(v.xaxis)[1:(x.br)-1] >1)) + 1):x.br]

      # white is equal to 1 seeing how many have values are higher than bg_lightness
      v.rowsums_yaxis <- rowSums(1-fig.l[,thick_y_axis] > bg_lightness)
      y_axis_end = tail(which(diff(v.rowsums_yaxis) != 0),2)[1]

      fig_l_new[thick_x_axis,thick_y_axis[1]:x_axis_end] = 0

      fig_l_new[thick_x_axis[1]:y_axis_end,thick_y_axis] = 0


      thickness = length(thick_y_axis)

      topdown = c(c(1:thickness),c(nrow(fig_l_new) - thickness + 1):nrow(fig_l_new))

      leftright = c(c(1:thickness),c(ncol(fig_l_new) - thickness + 1):ncol(fig_l_new))

      fig_l_new[topdown,leftright] = 1

      step_1[,,3] = fig_l_new


      xaxis = xaxis[xaxis< x_axis_end]
      yaxis = yaxis[yaxis< y_axis_end]

          }


  }else{
    xaxis = xaxis0.9
    yaxis = yaxis0.9
  }



  return(list(step_1 = step_1, fig.hsl = fig.hsl[yaxis,xaxis,], axes=list(xaxis = xaxis, yaxis = yaxis)))
}

