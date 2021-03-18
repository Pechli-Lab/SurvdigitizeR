fun_intobox <- function(step5.lst){
  # removing background colour
  white_img <-which.min(rowSums(abs(step5.lst$grs[,4:6]-1)))
  step5.lst$col_fig[[white_img]] <- NULL
  
  # number of colors
  num_col <- length(step5.lst$col_fig)
  
  # for each possible colr
  res_list <-vector(mode = "list", length = num_col)
  
  for(col1 in 1:num_col){
    
    pxl.col <- step5.lst$col_fig[[col1]] # Color figure
    pxl.grey <- grayscale(pxl.col) < 0.95 & step5.lst$fig$lineloc  # grey figure
    mat.grey <- pxl.grey[,,1,1] # turning into a matrix
    mat.grey <- t(mat.grey) # so that when loaded into 
    d.mat.grey <- dim(mat.grey)
    
    # turning into a data.frame
    
    r1  <- lapply(1:dim(mat.grey)[2], function(x1){
      x <- mat.grey[,x1]
      temp1  <- which(c(x[-1],F) != c(F,x[-length(x)]))
      if(length(temp1) >0){
        
        c(x = x1 ,start = max(temp1), end = min(temp1), consistent = all(diff(temp1)  == 1))
      }
      
    }) %>% 
      bind_rows() 
    
    # creating a length variable, xs and xend, n1 = lead of ys, and n2 lead of ye (trying to create boxes)
    # than diffing if no difference than same group (0), cumsum to detect groups
    r2 <- r1 %>% 
      mutate(length = start-end,
             xs = x, xe = x, ys = start, ye = end) %>% 
      mutate(n1 = c(0,diff(ys)), n2 = c(0,diff(ye))) %>% 
      mutate(r = cumsum(abs(n1) + abs(n2) != 0  )) %>% 
      group_by(r) %>%
      summarise(xs = min(xs), xe = max(xe), 
                ys = min(ys), ye = max(ye),
                consistent = max(consistent))
    
    # going to try and merge groups
    J <- dim(r2)[1]
    while(J > 0){
      r2 <-  r2 %>% 
        mutate(n1 = c(0,diff(ys)), n2 = c(0,diff(ye))) %>% 
        mutate(r = cumsum(abs(n1) + abs(n2) != 0  )) %>% 
        group_by(r) %>%
        summarise(xs = min(xs), xe = max(xe), 
                  ys = min(ys), ye = max(ye),
                  consistent = max(consistent))
      
      J <- J -  dim(r2)[1]
    }
    
    # creating mid (midpoint of two curves, k for ones withlout issues)
    r2 <-r2 %>% 
      mutate(mid = (ys+ye)/2) %>% 
      mutate(k = lead(mid, default = min(mid)) <= mid,
             pos_cen = F) 
    
    # working on those without issues 
    tofix <-which(!r2$k)
    
    for(work1 in tofix){
      
      if(all(r2$ys[c(work1-1, work1+1)] > r2$ys[work1])){ # in the case where next one have the same ys
        # posible censoring
        
        r2$pos_cen[work1] <- T
        
      } else if(all(diff(r2[c(work1, work1+1),"ys"][[1]]) == 0)) {
        
        # another weird case i found
        r2[work1+1,"ye"] <- r2[work1,"ye"]
        r2$k[work1] <- T
      } else{
        message("new error in event finder")
        
      }
      
    }
    r2 <-r2 %>% 
      mutate(mid = (ys+ye)/2) 
    res_list[[col1]] <- r2 # returning back
    
  }
  
  return(res_list)
}
