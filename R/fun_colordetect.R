#' fun_colordetect
#' detects the location of each curve in the image using kmeans
#' @param fig.list output from fun_cleanplot
#' @param num_curves the number of curves that are on the figure to be digitized
#'
#' @return comp1: a data frame with y,x,R,G,B color values and curve associated
#' @export
#'
#' @examples # fun_colordetect(fig.list =  fig.list, num_curves = 3)
fun_colordetect <- function(fig.list, num_curves, HSV_colspace = F, BW_colspace = F){
  # detecting white space, as well as curves
  cent <- num_curves + 1
  new_array <-fig.list
  require("gdata")

  # creating a long matrix with the y,x and R,G,B values of every point in our curve
  v1 <- names(unmatrix(new_array[,,1]))
  col1 <- str_split(string = v1,pattern = ":",simplify = T)
  col1[,1] <- as.numeric(str_remove_all(col1[,1],"r"))
  col1[,2] <- as.numeric(str_remove_all(col1[,2],"c"))

  comp1 <-data.frame(y = as.numeric(col1[,1]),
                     x = as.numeric(col1[,2]),
                     R = as.vector(new_array[,,1]),
                     G = as.vector(new_array[,,2]),
                     B = as.vector(new_array[,,3]))

  if(HSV_colspace){
    ar.h <- matrix(data = 0, nrow = dim(new_array)[1], ncol = dim(new_array)[2])
    ar.s <- matrix(data = 0, nrow = dim(new_array)[1], ncol = dim(new_array)[2])
    ar.v <- matrix(data = 0, nrow = dim(new_array)[1], ncol = dim(new_array)[2])

    for(i in 1:dim(new_array)[2]){
      temp  <- rgb2hsv(new_array[,i,1],new_array[,i,2],new_array[,i,3])

      ar.h[,i] <- temp[1,]
      ar.s[,i] <- temp[2,]
      ar.v[,i] <- temp[3,]


    }

    comp1 <-data.frame(y = as.numeric(col1[,1]),
                       x = as.numeric(col1[,2]),
                       H = as.vector(ar.h),
                       S = as.vector(ar.s),
                       V = as.vector(ar.v))


  }


  if(HSV_colspace){
    ar.h <- matrix(data = 0, nrow = dim(new_array)[1], ncol = dim(new_array)[2])
    ar.s <- matrix(data = 0, nrow = dim(new_array)[1], ncol = dim(new_array)[2])
    ar.v <- matrix(data = 0, nrow = dim(new_array)[1], ncol = dim(new_array)[2])

    for(i in 1:dim(new_array)[2]){
      temp  <- rgb2hsv(new_array[,i,1],new_array[,i,2],new_array[,i,3])

      ar.h[,i] <- temp[1,]
      ar.s[,i] <- temp[2,]
      ar.v[,i] <- temp[3,]


    }

    comp1 <-data.frame(y = as.numeric(col1[,1]),
                       x = as.numeric(col1[,2]),
                       H = as.vector(ar.h),
                       S = as.vector(ar.s),
                       V = as.vector(ar.v))


  }




  if(BW_colspace){
    Clinear <-  0.2126*new_array[,,1] + 0.7152*new_array[,,2] + 0.0722*new_array[,,3]


    comp1 <-data.frame(y = as.numeric(col1[,1]),
                       x = as.numeric(col1[,2]),
                       BW = as.vector(Clinear))


  }



  # running kmeans algorithm to group into colours based on number of curves
  out1 <- kmeans(x = comp1[,-c(1:2)], centers =cent)

  # remove group which is closest to white
  gr_white <-which.min(rowSums((out1$centers - 1 )^2))


  if(HSV_colspace | BW_colspace){
    gr_white <- which.max(table(out1$cluster))

  }

  # add group to original data.frame
  comp1$group <- out1$cluster

  comp1 <-comp1 %>%
    filter(group != gr_white)

  # remove all cases where group != gr_white.
  return(comp1)
}
