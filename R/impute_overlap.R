#' @title Impute Overlapping Values
#' @description This function assists in resolving issues related to color curve overlaps that occur during the initial phases of a Kaplan-Meier (KM) study.
#' @param fig.curves A dataframe with x, y, and group values. Default value is 'step4'.
#' @param size An integer that specifies the size of number of time interval for imputation.. Default value is 50.
#' @return A vector with a dataframe for each curve, stored in 'res_list'.
#' @examples
#' # impute_overlap(fig.curves, size = 50)
#' @export
impute_overlap<- function(fig.curves = step4, size = 50){


  if(length(unique(fig.curves$group)) == 2){

  num_window = size
  ratio_tol = 4 #tolearance of overlap ratio
  xrange = range(fig.curves$x)

  g1 = fig.curves[fig.curves$group == 1,]

  g1 = rbind(data.frame(x = min(fig.curves$x),y = max(fig.curves$y), group = 1),g1)

  g2 = fig.curves[fig.curves$group == 2,]
  g2 = rbind(data.frame(x = min(fig.curves$x),y = max(fig.curves$y), group = 2),g2)

  xrange = range(c(g1$x,g2$x))
  win_size = (xrange[2] - xrange[1])/num_window

  for(i in c(0:(num_window/4))){

  xstart = xrange[1] + i * win_size
  lg1 = length(g1[g1$x >=xstart & g1$x < (xstart + win_size),]$y)
  lg2 = length(g2[g2$x >=xstart & g2$x < (xstart + win_size),]$y)

  #print(paste0("g1 length:", lg1, "  g2 length:", lg2))

   if( min(lg1,lg2) == 0 |  (max(lg1,lg2)/min(lg1,lg2)) > ratio_tol ){
    #print("rep")
    max_cuv = which.max(c(lg1,lg2))
    if(max_cuv == 1){

    rep = g1[g1$x >=xstart & g1$x < (xstart + win_size) & g1$y > max(g2[g2$x >= (xstart + win_size),]$y) & g1$y < min(g2[g2$x < xstart,]$y),]
    if (nrow(rep) > 0){ rep$group = 2}
    g2 = g2[g2$x < xstart | g2$x >= (xstart + win_size),]
    g2 = rbind(g2,rep)

   # g2 %>%
   #    ggplot(aes(x =x ,y =y, color = as.factor(group))) +
   #    geom_point(size = 0.01) +
   #    theme_bw()

    }else{

      rep = g2[g2$x >= xstart & g2$x < (xstart + win_size) & g2$y > max(g1[g1$x >= (xstart + win_size),]$y) & g2$y < min(g1[g1$x < xstart,]$y),]
      if (nrow(rep) > 0){ rep$group = 1}
      g1 = g1[g1$x < xstart | g1$x >= (xstart + win_size),]
      g1 = rbind(g1,rep)

    }


   }


  }
  fig.curves = rbind(g1,g2)
  reorder <- order(fig.curves$group,fig.curves$x,-fig.curves$y)
  fig.curves = fig.curves[reorder,]

  }else if(length(unique(fig.curves$group)) == 3){


    g1 = fig.curves[fig.curves$group == 1,]
    g1 = rbind(data.frame(x = min(fig.curves$x),y = max(fig.curves$y), group = 1),g1)

    g2 = fig.curves[fig.curves$group == 2,]
    g2 = rbind(data.frame(x = min(fig.curves$x),y = max(fig.curves$y), group = 2),g2)

    g3 = fig.curves[fig.curves$group == 3,]
    g3 = rbind(data.frame(x = min(fig.curves$x),y = max(fig.curves$y), group = 3),g3)

    fig.curves = rbind(g1,g2,g3)
    reorder <- order(fig.curves$group,fig.curves$x,-fig.curves$y)
    fig.curves = fig.curves[reorder,]


}


  return(fig.curves)
}
