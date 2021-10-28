#' img_read
#' loads a jpeg or jpg or PNG file and turns into into arrays for further processing.
#' @param path character string indicating the location of the curve to be digitized
#'
#' @return fig.hsl array of pixels in HSL format (hue/saturation/lightness)
#' @export
#'
#' @examples  # img_read(path = "~/curve1.jpeg")
#'
img_read <- function(path){
  # To do:
  # test w/ more curves

  ## Checking if File-exists letting user know if not
  if(!file.exists(path)){
    stop(paste0("there is no file titled:", path, " \n Are you sure the file path is spelled correctly."))
  }


  # checking if jpeg,jpg, or png
  if(stringr::str_detect(path , "jpeg|jpg")){
    fig.arr <- jpeg::readJPEG(path,native = F)
  } else if(stringr::str_detect(path , "png")){
    fig.arr <-  png::readPNG(path,native = F)
  } else(
    stop("Couldn't read in image are you sure the file is saved as a jpeg, jpg, or png?")
  )

  ## Turning BW into an RGB image
  if(length(dim(fig.arr)) == 2 ){
    temp <- array(dim = c(dim(fig.arr),3))
    temp[,,1] <- fig.arr
    temp[,,2] <- fig.arr
    temp[,,3] <- fig.arr
    fig.arr <- temp
    rm(temp)
  }

  # Converting RBGA images into RGB
  if(dim(fig.arr)[3] > 3){
    fig.arr <-  fig.arr[,,-c(4:dim(fig.arr)[3])]
  }


  # Finally, converting RGB to HSL
  # https://stackoverflow.com/a/58426404
  color.min <- pmin(fig.arr[,,1], fig.arr[,,2], fig.arr[,,3])
  color.max <- pmax(fig.arr[,,1], fig.arr[,,2], fig.arr[,,3])
  color.delta <- color.max - color.min

  # calculating hue
  ar.h <- matrix(data = 0, nrow = dim(fig.arr)[1], ncol = dim(fig.arr)[2])

  # calculating hue for Max == Red
  rmax.mat <- which(color.delta != 0 & color.max == fig.arr[,,1], arr.ind=T)
  rmax_g <- fig.arr[cbind(rmax.mat, rep(2, dim(rmax.mat)[1]))]
  rmax_b <- fig.arr[cbind(rmax.mat, rep(3, dim(rmax.mat)[1]))]
  ar.h[rmax.mat] <-((rmax_g - rmax_b) / color.delta[rmax.mat]) %% 6

  # calculating hue for Max == Green
  gmax.mat <- which(color.delta != 0 & color.max == fig.arr[,,2], arr.ind=T)
  gmax_b <- fig.arr[cbind(gmax.mat, rep(3, dim(gmax.mat)[1]))]
  gmax_r <- fig.arr[cbind(gmax.mat, rep(1, dim(gmax.mat)[1]))]
  ar.h[gmax.mat] <-((gmax_b - gmax_r) / color.delta[gmax.mat]) + 2

  # calculating hue for Max == Blue
  bmax.mat <- which(color.delta != 0 & color.max == fig.arr[,,3], arr.ind=T)
  bmax_r <- fig.arr[cbind(bmax.mat, rep(1, dim(bmax.mat)[1]))]
  bmax_g <- fig.arr[cbind(bmax.mat, rep(2, dim(bmax.mat)[1]))]
  ar.h[bmax.mat] <-((bmax_r - bmax_g) / color.delta[bmax.mat]) + 4

  ar.h <- round(ar.h*60)

  # calculating lightness
  ar.l <- (color.max + color.min) / 2

  # calculating saturation
  ar.s <- color.delta / (1 - abs(2*ar.l - 1))
  ar.s[is.na(ar.s)] <- 0

  fig.hsl <- array(dim = dim(fig.arr))
  fig.hsl[,,1] <- ar.h
  fig.hsl[,,2] <- ar.s
  fig.hsl[,,3] <- ar.l

  # Weird thing about raster images is that they read upside so have to flip on the horizontal axis
  fig.hsl <- fig.hsl[dim(fig.hsl)[1]:1,, ]


  return(fig.hsl)
}



