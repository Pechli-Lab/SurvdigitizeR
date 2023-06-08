#' color_cluster
#' groups pixels of each curve in the image based on color using k-medoids clustering
#' @param fig.df a dataframe with x,y and h,s,l values for each pixel
#' @param num_curves the number of curves that are on the figure to be digitized
#' @param censoring  logical indicating whether censoring occurs in a different color (usually black)
#' @param enhance indicating whether converting HSl channcels into same scale
#'
#' @return fig.grp: a dataframe with x,y,h,s,l values and associated group (curve) for each pixel
#' @importFrom cluster clara
#' @export
#'
#' @examples # color_cluster(fig.df =  fig.df, num_curves = 3, censoring = F, enhance = F)
color_cluster <- function(fig.df, num_curves = 3, censoring = F, enhance = F){

  # if censoring then remove darkest color
  if(censoring){
    fig.df <- fig.df[fig.df$l >= 0.2,]
  }

  # running cluster algorithm to group into colours based on number of curves

  # increase the effect of the hue and lightness components
  in1 <- fig.df[,c('h', 's', 'l')]

  # increase the effect of the hue and lightness components
  in2 <- fig.df[,c('h', 's', 'l')]
  in2[,'s'] <- in2[,'s'] * 100
  in2[,'l'] <- in2[,'l'] * 100

  if (enhance == T){
    # medoids
    out1 <- cluster::clara (x = in2, sampsize = 500, k = num_curves,
                            samples = 50, pamLike = T)
  } else {
    # medoids
    out1 <- cluster::clara (x = in1, sampsize = 500, k = num_curves,
                            samples = 50, pamLike = T)
  }
  centerpoints <- out1$medoids
  sizes <- out1$clusinfo[,'size']

  fig.grp <- fig.df[,c('x','y')]
  fig.grp$group <- out1$clustering

  return(fig.grp)
}
