#' fig_summarize
#' Brings together the curve and range data from into a format that is readable by the survival function
#' @param lines_vector: output from lines_isolate
#' @param range_list : output from range_detect
#'
#' @return # a dataframe with id, times, St, curve
#' @export
#'
#' @examples # fig_summarize(lines_vector, range_df)
fig_summarize <-function(lines_vector,range_list){

  require(tidyr)
  lines_vector <- bind_rows(lines_vector)


  if(range_list$X_0pixel < min(lines_vector$x)){
    lines_vector$x <-(lines_vector$x-range_list$X_0pixel)*range_list$x_increment

  } else{
    lines_vector$x <-(lines_vector$x)*range_list$x_increment
  }

  if(max((lines_vector$y -range_list$Y_0pixel )*range_list$y_increment)   < 0.95){

    lines_vector$y <-  (lines_vector$y )*range_list$y_increment

  } else{
    lines_vector$y <-  (lines_vector$y - range_list$Y_0pixel)*range_list$y_increment
  }

  # if(max(lines_vector$y ) > 1){
  #   lines_vector$y <- lines_vector$y + (1-max(lines_vector$y ))
  # }
  if(min(lines_vector$x ) < 0){

    lines_vector$x <- lines_vector$x + abs(min(lines_vector$x ))

  }
  out1 <-lines_vector %>%
    group_by(curve) %>%
    mutate(d1 = cumsum(c(0, diff(y)) != 0)) %>% ungroup() %>%
    group_by(curve,d1) %>%
    summarise(y = first(y),
              xmin = min(x),
              xmax = max(x),
              curve= unique(curve)) %>%
    ungroup() %>%
    group_by(curve) %>%
    mutate(d1 = 1:n()) %>%
    pivot_longer(cols = c("xmin","xmax")) %>%
    arrange(curve,d1, desc(y)) %>%
    group_by(curve) %>%
    mutate(id = 1:n()) %>%
    rename(St = y,
           time = value ) %>%
    select(id, time, St, curve) %>%
    ungroup() %>% mutate(St = if_else(St < 0, 0, St))
  # adding last point

  #  lpoint <-lines_vector %>%
  #    group_by(curve) %>%
  #    summarise(time = last(x), St = last(y_min)) %>% ungroup() %>%
  #    mutate(St = if_else(St < 0, 0, St))
  #
  #
  #  out1 <- out1 %>% bind_rows(lpoint) %>% arrange(curve, time) %>% mutate(id = 1:n())

  return(out1)
}

