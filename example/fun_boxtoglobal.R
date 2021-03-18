fun_boxtoglobal <- function(step6.df,glbsen){
  # requiers
  # step6.df <- res.fun_intobox$Curve_1

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  n_gr <- unique(step6.df$group)
  df.list <- vector(length = length(n_gr), mode = "list")
  
  # creating a global group attribute and removing global groups that dont cover 0.33 of the y axis and 0.2 of the xaxis
  for( ri in 1:length(n_gr)){
    
    step6.df_gr <- step6.df[step6.df$group == n_gr[ri],]
    step6.df_gr <- step6.df_gr %>% 
      mutate(group_glob= 1:n())
    
    cycle.i <- 1  
    retry.i <- 1
    
    # n <- gr 
    #while(T){
    
    while(T){
      n.gri <- unique(step6.df_gr$group_glob)
      l.gr1 = length(n.gri)
     # message(paste("\r","interation",cycle.i, "num groups", l.gr1 ))
      cycle.i <- cycle.i + 1 # update cycle
      for( j in n.gri){
        mem.curr.group <- which(step6.df_gr$group_glob == j)
        l.curr.group <- length(mem.curr.group)
        x.curr.group <- step6.df_gr$x[mem.curr.group]
        ye.curr.group <- step6.df_gr$ye[mem.curr.group]
        ys.curr.group <- step6.df_gr$ys[mem.curr.group]
        if(l.curr.group> 0){
          for(k in 1:length(l.curr.group)){
            
            r1 <-  which((abs(step6.df_gr$x - x.curr.group[k])  < glbsen ) & ( abs(step6.df_gr$ye - ye.curr.group[k] ) < glbsen |
                                                                             abs(step6.df_gr$ys - ys.curr.group[k] ) < glbsen ) &
                           step6.df_gr$group_glob != j              
            )
            
            gr_to_rep <- step6.df_gr$group_glob[r1]
            step6.df_gr$group_glob[step6.df_gr$group_glob %in% gr_to_rep] <- j
            
            
          }
        }
        
        
        
        
      }
      
      # Control structure 
      if(length(unique(step6.df_gr$group_glob))== l.gr1 ){ 
        retry.i <- retry.i + 1
        if(retry.i > 2){
          break
        }
      } else {
        retry.i <-  1
      }
      
      if(cycle.i == 1000){break} # emergency break
      
    }
    
    step6.df_gr1 <- step6.df_gr %>% group_by(group_glob) %>% 
      mutate(minx = min(x),
             maxx=  max(x),
             miny = min(c(ye,ys)),
             maxy = max(c(ys,ye))) %>% ungroup() %>% 
      mutate(r1 = case_when(maxx-minx < max(step6.df_gr$x)*0.33 &
                              maxy-miny < max(step6.df_gr$ys)*0.2  ~ F,
                            T ~ T
      )) 
    step6.df_gr <- step6.df_gr1 %>% filter(r1) %>% select(x,ye,ys,gr) %>% left_join(step6.df_gr)
    df.list[[ri]] <-   step6.df_gr
    
    
    
  }
  
  
  df.list2 <- do.call(rbind,df.list)
  
  return(df.list2)
  
  
}
