#' @title Figure Summarization
#' @description Integrates the curve and range data into a format that is readable by the survival function.
#' @param lines_vector The output from the `lines_isolate` function.
#' @param range_list The output from the `range_detect` function.
#' @param y_start Starting value of the y-axis, defaults to the global `y_start` variable.
#' @param y_end Ending value of the y-axis, defaults to the global `y_end` variable.
#' @return A dataframe with columns: 'id', 'times', 'St', and 'curve'.
#' @examples
#' # fig_summarize(lines_vector, range_list, y_start, y_end)
#' @export
#'
fig_summarize <-function(lines_vector,range_list,y_start = 0,y_end = 1){

  require(tidyr)
  lines_vector <- bind_rows(lines_vector)

  x_diff = min(lines_vector$x) - range_list$X_0pixel

  lines_vector$x = lines_vector$x - x_diff

  if(range_list$X_0pixel <= min(lines_vector$x)){
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


  num_cuv = length(unique(out1$curve))
  for(i in c(1:num_cuv)){
    select_cuv = out1[out1$curve == i,]
    st = select_cuv$St - (max(select_cuv$St) - y_end)
    st[select_cuv$St == y_start & st != y_start ] = y_start
    out1[out1$curve == i,]$St = st
  }


  return(out1)
}

