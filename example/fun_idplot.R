# fun_idplot.R

fun_idplot  <- function(ls.fig, i.sen = 0.05){
# Input:
  # ls.fig : list object created from fun_readsurv.R
  # i.sen: a double between 0 and 1 indicating sensitivity (What doesn't count as white space )
            # 0 any value > 0 , 1 no value is a color 
# Output: 
  # an list w/ two objects
    # ls.figcut: a list of three figure items in 
    # xaxis: a vector of integers to index plot by collumns
    # yaxis: a vector of integers to index plot by rows
# Dependency:
  #
# To do
  # - make sure locations of zeroes doesnt break 
  # - test w/ other curves
  # - possible for curve to be boxed in. create statement for that (ie v.rowsums)

  
# saving as fit.BW  
  fig.BW  <- ls.fig$fig.BW
  # white is equal to 1 seeing how many have values
  
  v.colsums <- colSums(1-fig.BW > i.sen)
  # idenftifying rowSums number of pixels where 1-pixel > i.sen
  v.rowsums <- rowSums(1-fig.BW > i.sen)
  
  # identify all the collumns w/ rowsums close to our max 
  v.xaxis <-  which(v.colsums[which.max(v.colsums)]*0.95 < v.colsums)
  
  
  # going to try and fix w/ the following if statement
  if(any(diff(v.xaxis) > dim(fig.BW)[2]*0.4)){
    # Two xaxis
    x.br <- which(diff(v.xaxis) > dim(fig.BW)[1]*0.3)
    x1 <- v.xaxis[1:x.br]
    x2 <- v.xaxis[-c(1:x.br)]
    
    xaxis <-  (max(x1) + 1):(min(x2)-1)
    
  } else {
    
    i.xaxis <- max(v.xaxis)
    lc_zeroes_x <- which(v.colsums == 0 & lag(v.colsums, default = 0) == 0) 
    suppressWarnings(i.xaxis.end <- min(lc_zeroes_x[(lc_zeroes_x > i.xaxis)]))
    if(is.finite(i.xaxis.end)){
      xaxis <- (i.xaxis+1):(i.xaxis.end-1)
    } else {
      xaxis <- (i.xaxis+1):length(v.colsums)
    }
  } 
  
  
  
  # identify all the rows w/ rowsums close to our max 
  v.yaxis <-  which(v.rowsums[which.max(v.rowsums)]*0.95 < v.rowsums)
  
  if(any(diff(v.yaxis) > dim(fig.BW)[1]*0.4)){
    # Two xaxis
    y.br <- which(diff(v.yaxis) > dim(fig.BW)[1]*0.3)
    y1 <- v.yaxis[1:y.br]
    y2 <- v.yaxis[-c(1:y.br)]
    
    yaxis <-  (max(y1) + 1):(min(y2)-1)
    
  } else {
    
    
    i.yaxis <- max(v.yaxis)
    
    # y axis starts at i.yaxis+1 and xaxis starts att i.xaxis +1
    # have the issue that might have a lot of 0's at the end want to crop them out
    
    lc_zeroes_y <- which(v.rowsums == 0 & lag(v.rowsums, default = 0) == 0) 
    suppressWarnings(i.yaxis.end<- min(lc_zeroes_y[(lc_zeroes_y > i.yaxis)]))
    
    if(is.finite(i.yaxis.end)){
      yaxis <- (i.yaxis+1):(i.yaxis.end-1)
    } else{   
      yaxis <- (i.yaxis+1):length(v.rowsums)
    }
  }
  
 
 

  ls.fig$fig.arr <-  ls.fig$fig.arr[yaxis,xaxis,]
  ls.fig$fig.hsv <-  ls.fig$fig.hsv[yaxis,xaxis,]
  ls.fig$fig.BW  <-  ls.fig$fig.BW[yaxis,xaxis]
 
 return(list(fig = ls.fig , axis= list(yaxis = yaxis, xaxis = xaxis)))
}

  



# test
#fig.BW <- step1$fig.BW
#i.sen = 0.05
#
#res <- fun_idplot(fig.BW = step1$fig.BW, i.sen = 0.05)
#res1 <- step1$fig.BW[res$yaxis,res$xaxis]
#require(dplyr)
#require(tidyr)
#require(stringr)
#require(ggplot2)
#
#as.data.frame(res1) %>% 
#  mutate(y = 1:n()) %>% 
#  gather(-y, key ="X", value = "pixel") %>% 
#  mutate(X = as.numeric(str_remove(X, "V"))) %>% 
#  filter(pixel < 0.95) %>% 
#  ggplot(aes(x = X, y = y)) + geom_point()
