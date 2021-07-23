#' get_tofinal
#' removes points that don't fit the structure of a survvival curve based on e.gen
#' @param res_list : output from overlap_detect
#'
#' @return res_vect: a dataframe that complies with the values of a survival curve
#' @export
#'
#' @examples # get_tofinal(res_list)
get_tofinal <- function(res_list){
  # save results in res_vector
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
