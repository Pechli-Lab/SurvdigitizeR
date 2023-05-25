#' color_cluster
#' groups pixels of each curve in the image based on color using k-medoids clustering
#' @param fig.df a dataframe with x,y and h,s,l values for each pixel
#' @param num_curves the number of curves that are on the figure to be digitized
#' @param censoring  logical indicating whether censoring occurs in a different color (usually black)
#'
#' @return fig.grp: a dataframe with x,y,h,s,l values and associated group (curve) for each pixel
#' @export
#'
#' @examples # color_cluster(fig.df =  fig.df, num_curves = 3, censoring = F)
color_cluster <- function(fig.df, num_curves = 3, censoring = F){

  require("gdata", warn.conflicts=FALSE)
  library("cluster")
  library(data.table)

  # if censoring then remove darkest color
  if(censoring){
    fig.df[fig.df$l < 0.2,]
  }

  # running cluster algorithm to group into colours based on number of curves

  # kmeans
  # out1 <- kmeans(x = comp1[,-c(1:2)], centers = num_curves, nstart=3)
  # centerpoints <- out1$centers[,-4]
  # sizes <- out1$size
  # cluster_data <- out1$cluster

  # increase the effect of the hue and lightness components
  in1 <- fig.df[,c('h', 's', 'l')]

  # medoids
  out1 <- clara (x = in1, sampsize = 500, k = num_curves,
                 samples = 50, pamLike = T)
  centerpoints <- out1$medoids
  sizes <- out1$clusinfo[,'size']

  fig.grp <- fig.df[,c('x','y')]
  fig.grp$group <- out1$clustering

  return(fig.grp)
}

