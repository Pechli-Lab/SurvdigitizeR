#' lines_isolate
#' Select the final singular y values for each curve (Note: this is where additional event detection could be performed)
#' @param fig.curves : a dataframe with x, y and group values
#'
#' @return res_list: a vector with a dataframe for each curve
#' @export
#'
#' @examples # lines_isolate(fig.curves)
lines_isolate <- function(fig.curves){

  # getting ymax and ymin, setting y to be the difference
  res.df2 <- fig.curves
  res.df2$curve <- res.df2$group
  res.df2 <- res.df2 %>%
    group_by(x, curve) %>%
    summarise(y = max(y), .groups = 'drop')


  # splitting dataframe to list of <#curves> dataframes
  res_list <- res.df2%>% split(., .[, 'curve'])

  # converting to vector, making sure everything is above y=0
  res_vect <- vector(mode = "list",length = length(res_list))
  for( j in seq_along(res_list)){
    # for each curve

    t1 <- res_list[[j]]
    t1 <- t1 %>%
      filter(y > 0)

    res_vect[[j]] <- t1


  }

  return(res_vect)
}
