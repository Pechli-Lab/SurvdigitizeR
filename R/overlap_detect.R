#' overlap_detect
#' this function detects potential overlap between curves
#' @param step4 : output from fun_colordetection
#'
#' @return res_list: a dataframe with the detected values for all curves as well as potential overlap
#' @export
#'
#' @examples # overlap_detect(df_colordetect)
overlap_detect <- function(step4){

  # create groups based on continousy values
  comp3 <- step4 %>%
    group_by(x,group) %>%
    mutate(diff_y = c(1,diff(y) != 1 )) %>% ungroup() %>%
    mutate( gr_y = cumsum(diff_y)) %>%
    group_by(gr_y) %>%
    mutate(y_min = min(y),
           y_max = max(y),
           x = unique(x),
           group = unique(group))

  # storing all values in res_list
  res_list <- vector(mode  = "list", length = length(unique(step4$group)))
  n1 <- 1
  for(i in unique(step4$group)){
    # for each curve
    # create act1 and temp1 for the values of each group
    act1 <- comp3 %>% filter(group == i)
    temp1 <- comp3 %>% filter(group == i)
    # get the median drop value in f6
    f6 <-median(abs(temp1$y_min- temp1$y_max))
    # potential == 6 indicates that these are the oroginal values
    temp1$potential <- 6
    act1$potential <-6
    temp2 <- comp3 %>% filter(group != i)
    # find all other groups not based on y that overlap
    for(j in unique(temp2$gr_y)){
      # Possible cases directly above or directly below
      # check if they share an x,
      vert_overlap <- FALSE
      hor_overlap <- FALSE
      a1 <-temp2[temp2$gr_y == j,]
      ### See if any of the temp == a1 x
      if(any(temp1$x ==a1$x[1])){
        vert_overlap <- any(unlist(lapply(a1$y, function(x){ any(abs(x- temp1[temp1$x ==a1$x[1],"y"]  ) ==1 )  })))
      }

      if(any(((temp1$x-1) ==a1$x[1] )| ((temp1$x+1) ==a1$x[1] )  )){
        hor_overlap <-any(unlist(lapply( temp1[(((temp1$x-1) ==a1$x[1] )| ((temp1$x+1) ==a1$x[1] )  ),"y",drop = T], function(x){any(x == a1$y) })))
      }

      a1$potential <- hor_overlap + vert_overlap
      # See if x-1 or x + 1 have y matches call horizontal overlap

      if( hor_overlap + vert_overlap > 0 & abs(min(a1$y)- max(a1$y)) <  f6*2 ){
        act1 <- rbind(act1, a1)
      }

    }
    act1$curve <- n1

    res_list[[n1]] <- act1
    n1 <- n1 + 1 # manually adding this
  }

  return(res_list)
}



