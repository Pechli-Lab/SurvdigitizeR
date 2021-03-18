#' eventdetect
#' detects potential events
#' @param res_list : output from overlap_detect
#'
#' @return res_list2: a dataframe with potential x, and y values
#' @export
#'
#' @examples # eventdetect(overlap_detectout)
eventdetect <- function(res_list){
  # saving results to res_list2
  res_list2 <- vector(mode = "list", length = length(res_list))
  # for each curve
  for(i in seq_along( res_list2)){
  # detect average drop
    avg_drop <-median(res_list[[i]]$y_max- res_list[[i]]$y_min)
  # using group_by to generate y_min and y_max values
    res_list2[[i]]  <- res_list[[i]] %>%
      distinct(group, x,  gr_y, y_min,y_max, potential) %>% ungroup() %>%
      arrange(x) %>%
      group_by(x) %>%
      summarise(y_min = min(y_min),
                y_max = max(y_max),
                cor = all(potential == 6)) %>% ungroup() %>%
      mutate(y = y_max-avg_drop/2) %>%
      mutate(curve = i)

  }
  return(res_list2)
}
