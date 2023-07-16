#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @title Figure Summarization
#' @description Integrates the curve and range data into a format that is readable by the survival function.
#' @importFrom magrittr %>%
#' @param lines_vector The output from the `lines_isolate` function.
#' @param range_list The output from the `range_detect` function.
#' @param x_start The starting value of the x-axis.
#' @param y_start Starting value of the y-axis, defaults to the global `y_start` variable.
#' @param y_end Ending value of the y-axis, defaults to the global `y_end` variable.
#' @return A dataframe with columns: 'id', 'times', 'St', and 'curve'.
#' @examples
#' # fig_summarize(lines_vector, range_list, x_start, y_start, y_end)
#' @export
#'
fig_summarize <-function(lines_vector,range_list,x_start, y_start = 0,y_end = 1){

  lines_vector <- dplyr::bind_rows(lines_vector)

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

  if(min(lines_vector$x ) < 0){
    lines_vector$x <- lines_vector$x + abs(min(lines_vector$x ))
  }

  out1 <-lines_vector %>%
    dplyr::group_by(curve) %>%
    dplyr::mutate(d1 = cumsum(c(0, diff(y)) != 0)) %>% dplyr::ungroup() %>%
    dplyr::group_by(curve,d1) %>%
    dplyr::summarise(y = dplyr::first(y),
                     xmin = min(x),
                     xmax = max(x),
                     curve= unique(curve)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(curve) %>%
    dplyr::mutate(d1 = 1:dplyr::n()) %>%
    tidyr::pivot_longer(cols = c("xmin","xmax")) %>%
    dplyr::arrange(curve,d1, desc(y)) %>%
    dplyr::group_by(curve) %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::rename(St = y,
                  time = value ) %>%
    dplyr::select(id, time, St, curve) %>%
    dplyr::ungroup() %>% dplyr::mutate(St = dplyr::if_else(St < 0, 0, St))

  num_cuv = length(unique(out1$curve))
  for(i in c(1:num_cuv)){
    select_cuv = out1[out1$curve == i,]
    st = select_cuv$St   - (max(select_cuv$St) - y_end)
    st = dplyr::if_else(st > y_end, y_end, st)
    st[select_cuv$St == y_start & st != y_start ] = y_start
    if(st[1] != y_end){st[1] = y_end}
    st = dplyr::if_else(st < y_start, y_start, st)
    out1[out1$curve == i,]$St = st
    if (!(out1[out1$curve == i,][1,]$time == x_start & out1[out1$curve == i,][1,]$St == y_end)){
      new_id <- min(out1[out1$curve == i,]$id) - 1
      new_row <- data.frame(id = new_id, time = x_start, St = y_end, curve = i)
      out1 <- rbind(new_row, out1)
    }
  }
  # Reorder the dataframe by curve and id
  out1 <- out1 %>% dplyr::arrange(curve, id)
  out1 <- out1 %>% dplyr::group_by(curve) %>% dplyr::mutate(id = dplyr::row_number()) %>% dplyr::ungroup()

  return(out1)
}
