#' fun_summary
#' Brings together the data from get_final and fun_range into a format that is readable by the survival function
#' @param final_list: output from get_final
#' @param Step7_out : output from fun_range
#'
#' @return # a dataframe with id, times, St, curve
#' @export
#'
#' @examples # fun_summary(get_out, range_df)
fun_summary <-function(final_list,Step7_out){
require(tidyr)
  final_list <- bind_rows(final_list)


  #if(Step7_out$X_0pixel < min(final_list$x)){
    final_list$x <-(final_list$x-Step7_out$X_0pixel)*Step7_out$x_increment

  #} else{
  #  final_list$x <-(final_list$x)*Step7_out$x_increment
  #}

  if(max((final_list$y -Step7_out$Y_0pixel )*Step7_out$y_increment)   < 0.95){

    final_list$y <-  (final_list$y )*Step7_out$y_increment
    final_list$y_min <-  (final_list$y_min )*Step7_out$y_increment

  } else{
    final_list$y <-  (final_list$y - Step7_out$Y_0pixel)*Step7_out$y_increment
    final_list$y_min <-  (final_list$y_min - Step7_out$Y_0pixel)*Step7_out$y_increment
  }

  if(max(final_list$y ) > 1){
    final_list$y <- final_list$y + (1-max(final_list$y ))
    final_list$y_min <- final_list$y_min + (1-max(final_list$y ))
  }
  if(min(final_list$x ) < 0){

    final_list$x <- final_list$x + abs(min(final_list$x ))

  }
  out1 <-final_list %>%
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

#  lpoint <-final_list %>%
#    group_by(curve) %>%
#    summarise(time = last(x), St = last(y_min)) %>% ungroup() %>%
#    mutate(St = if_else(St < 0, 0, St))
#
#
#  out1 <- out1 %>% bind_rows(lpoint) %>% arrange(curve, time) %>% mutate(id = 1:n())

  return(out1)
}
