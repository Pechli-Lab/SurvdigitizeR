#' img_read
#' loads a jpeg or jpg or PNG file and turns into into arrays for further processing.
#' @param path character string indicating the location of the curve to be digitized
#'
#' @return fig.hsl array of pixels in HSL format (hue/saturation/lightness)
#' @export
#'
#' @examples  # img_read(path = "~/curve1.jpeg")
#'
img_read_rgb <- function(path){
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


  return(fig.arr)
}



