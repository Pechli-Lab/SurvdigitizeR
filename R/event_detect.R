#' eventdetect
#' detects potential events
#' @param res.df : a dataframe with x, y and group values
#'
#' @return res_list: a list with a dataframe for each curve
#' @export
#'
#' @examples # eventdetect(overlap_detectout)
eventdetect <- function(res.df){

  # getting ymax and ymin, setting y to be the difference
  res.df2 <- res.df
  res.df2$curve <- res.df2$group
  res.df2 <- res.df2 %>%
    group_by(x, curve) %>%
    summarise(y_min = min(y), y_max = max(y), .groups = 'drop')

  res.df2$y <- res.df2$y_max - res.df2$y_min

  # splitting dataframe to list of <#curves> dataframes
  res_list <- res.df2%>% split(., .[, 'curve'])

  return(res_list)
}
