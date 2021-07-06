#' e_gen
#' This is the function that for each y estimate generates how close the other values look likea survival curve if you were to remove that y value.
#' @param y: y values
#'
#' @return ret_v: a vector with the ith element indicating how close the other set of points resemble a survival curve on the removal of the y value
#' @export
#'
#' @examples # e_gen ( y = c(1,0.5,1,0.1))
e_gen <-function(y){

  ret_v <- vector(mode = "numeric",length = length(y))

  for(j in seq_along(y)){

    ret_v[j] <-  sum((y[-j] - cummin(y[-j]))  >0 )

  }
  return(ret_v)
}
