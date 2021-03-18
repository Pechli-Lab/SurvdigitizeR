#' e_gen
#' This is the function that for each y estimate generates how close the other values look likea survival curve if you were to remove that y value.
#' @param y: y values
#' @param cor1: wether overlap or original y values
#'
#' @return ret_v: a vector with the ith element indicating how close the other set of points resemble a survival curve on the removal of the y value
#' @export
#'
#' @examples # e_gen ( y = c(1,0.5,1,0.1), cor = c(T,T,F,T))
e_gen <-function(y,cor1){

  ret_v <- vector(mode = "numeric",length = length(y))

  for(j in seq_along(y)){

    ret_v[j] <-  sum(((y[-j] - cummin(y[-j]))  >0 )*if_else(cor1[-j], 1,0.3))

  }
  return(ret_v)
}
