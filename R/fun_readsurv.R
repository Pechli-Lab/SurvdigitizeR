#' fun_readsurv
#' loads a jpeg or jpg or PNG file and turns into into arrays for further processing.
#' @param FilePath character string indicating the location of the curve to be digitized
#'
#' @return List with three objects: fig.rbg (RBG array), fig.BW (BW array), fig.hsv array in HSV format
#' @export
#'
#' @examples  # fun_readsurv(FilePath = "~/curve1.jpeg")
#'
fun_readsurv <- function(FilePath){
# To do:
# allow for BW images
# test w/ more curves

## Checking if File-exists letting user know if not
if(!file.exists(FilePath)){
  stop(paste0("there is no file titled:", FilePath, " \n Are you sure the file path is spelled correctly."))
}

# checking if jpeg,jpg, or png
if(stringr::str_detect(FilePath , "jpeg|jpg")){
fig.arr <- jpeg::readJPEG(FilePath,native = F)
} else if(stringr::str_detect(FilePath , "png")){
fig.arr <-  png::readPNG(FilePath,native = F)
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

# Converting RBGA images into rbg
if(dim(fig.arr)[3] > 3){
    fig.arr <-  fig.arr[,,-c(4:dim(fig.arr)[3])]
}


# Converting to greyscale
# Using formula found in
# https://stackoverflow.com/questions/17615963/standard-rgb-to-grayscale-conversion
Clinear <-  0.2126*fig.arr[,,1] + 0.7152*fig.arr[,,2] + 0.0722*fig.arr[,,3]
fig.BW <- apply(Clinear, MARGIN = 2,
                function(x){ ret<-  ifelse(x <= 0.0031308 , 12.92*x, 1.055*x^(1/2.4) - 0.055)
                return(ret)}
                )


# Converting to HSV
ar.h <- matrix(data = 0, nrow = dim(fig.arr)[1], ncol = dim(fig.arr)[2])
ar.s <- matrix(data = 0, nrow = dim(fig.arr)[1], ncol = dim(fig.arr)[2])
ar.v <- matrix(data = 0, nrow = dim(fig.arr)[1], ncol = dim(fig.arr)[2])

for(i in 1:dim(fig.arr)[2]){
    temp  <- rgb2hsv(fig.arr[,i,1],fig.arr[,i,2],fig.arr[,i,3])

    ar.h[,i] <- temp[1,]
    ar.s[,i] <- temp[2,]
    ar.v[,i] <- temp[3,]


}

fig.hsv <- array(dim = dim(fig.arr))
fig.hsv[,,1] <- ar.h
fig.hsv[,,2] <- ar.v
fig.hsv[,,3] <- ar.s

# Weird thing about raster images is that they read upside so have to flip on the horizontal axis
fig.arr <- fig.arr[dim(fig.arr)[1]:1,, ]
fig.BW <- fig.BW[dim(fig.BW)[1]:1, ]
fig.hsv <- fig.hsv[dim(fig.hsv)[1]:1,, ]


return(list(fig.arr = fig.arr,
            fig.BW = fig.BW,
            fig.hsv= fig.hsv))
}


