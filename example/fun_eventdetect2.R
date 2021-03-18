pxl_todf <-function(step7df, ste){
  step7df <- step7df %>% 
    mutate(x = (x-ste$X_0pixel)*ste$x_increment,
           ye = (ye-ste$Y_0pixel)*ste$y_increment,
           ys = (ys-ste$Y_0pixel)*ste$y_increment) 
  step7df <-step7df %>% 
    mutate(ye = ye - (max(ye)-1),
           ys = ys - (max(ys)-1))
  
  
  step7df$ye_act <- apply(step7df[, c("ye","ys")], 1, min)
  step7df$ys_act <- apply(step7df[, c("ye","ys")], 1, max)
  return(step7df)
}

fun_eventdetect2  <-function(step9a){
  
curve_value  <- unique(step9a$group_glob)
res_list <-vector(mode = "list", length = length(curve_value))
for(i_main in seq_along(curve_value)){
  step9_temp <-step9a %>% 
    filter(group_glob == curve_value[i_main])
  
  non_x <- unique(step9_temp$x)
  # favouring continuity in trying to identify x's
  for(i in seq_along(non_x)[-1]){
    if(sum(step9_temp$x == non_x[i] ) == 1){
      # if only 1 x' do nothing 
    } else{
      # if there's more than 1 x 
      t1 <- step9_temp[step9_temp$x == non_x[i],] 
      # keep only the one closest to the next variable
      t1 <-t1[which.min(abs(t1$ys_act - step9_temp$ys_act[step9_temp$x == non_x[i-1]])),]
      
      step9_temp <-  rbind(step9_temp[step9_temp$x != non_x[i],] ,
                          t1)
    }
  }
  
## trying to remove weird points  
  step9_temp <-step9_temp %>% arrange(x) %>% 
    mutate(pys = lag(ys_act , n = 1, default = 1),
           pye = lag(ye_act, n = 1, default = 1),
           nys = lead(ys_act,n = 1, default = 0),
           nye  = lead(ye_act, n = 1, default = 0))
  
  
  j <- dim(step9_temp)[1]
  while(j > 0){
    step9_temp %>% 
      mutate(pys = lag(ys_act , n = 1, default = 1),
             pye = lag(ye_act, n = 1, default = 1),
             nys = lead(ys_act,n = 1, default = 0),
             nye  = lead(ye_act, n = 1, default = 0)) %>% 
      filter(!(ys_act < pys & ye_act == pye))  -> step9_temp
    
    j <- dim(step9_temp)[1]-j
    message(j)
  }
  
  

  j <- dim(step9_temp)[1]
  while(j > 0){
    step9_temp %>% 
      mutate(pys = lag(ys_act , n = 1, default = 1),
             pye = lag(ye_act, n = 1, default = 1),
             nys = lead(ys_act,n = 1, default = 0),
             nye  = lead(ye_act, n = 1, default = 0)) %>% 
      filter(!(ys_act < pys & nys > ys_act))  -> step9_temp
    
    j <- dim(step9_temp)[1]-j
    message(j)
  }
  
  step9_temp <- step9_temp %>% 
    mutate(pys = lag(ys_act , n = 1, default = 1),
           pye = lag(ye_act, n = 1, default = 1),
           nys = lead(ys_act,n = 1, default = 0),
           nye  = lead(ye_act, n = 1, default = 0)) %>% 
   # transmute(x =x , probs =ys) %>% 
    filter( ys_act - lag(ys_act,1,1) < 0.005)
  
  
  
  
  res_list[[i_main]] <- step9_temp
  
}

# dealing with overlap
if(length(res_list)>1){
  for( i in seq_along(res_list)){
    test3 <- res_list[[i]]
    for(j in seq_along(res_list)[-1]){
      
    test3a <-res_list[[j]]
      
    # merging curves 
    x_1 <-test3a$x
    x_2 <-test3$x
    
    pos_x2 <- x_1[!(x_1 %in% x_2)]
    pos_x1 <- x_2[!(x_2 %in% x_1)]
    
    can_goin <- c()
    for(j in seq_along(pos_x2)){
      # find closest x's that are bigger
      temp1 <- rbind(test3a[test3a$x == pos_x2[j],],
                     test3) %>% 
        arrange(x) %>% 
        mutate(e1 =   ys_act <= lag(ys_act,1,1) & ys_act >= lead(ys_act,1,0))
      
      if(temp1$e1[ temp1$x == pos_x2[j]]
         
      ){
        can_goin <- c(can_goin,pos_x2[j])
      }
      
      
    }
    
    test3 <- test3 %>%bind_rows(test3a[test3a$x %in% can_goin,]) 
    
    
    
    
    
    
    }
    
    res_list[[i]] <-test3
    
    
    
  }
  
  
## f  

  
}

## turning into format that they understand 
for( i in seq_along(res_list)){
  
  test5_ev <-res_list[[i]] %>% 
    arrange(x) %>% 
    mutate(pys = lag(ys_act , n = 1, default = 1),
           pye = lag(ye_act, n = 1, default = 1),
           nys = lead(ys_act,n = 1, default = 0),
           nye  = lead(ye_act, n = 1, default = 0)) %>% 
    filter(ys_act != pys & ye_act != pys )
  test5_ev %>% 
  mutate(pys = lag(ys_act , n = 1, default = 1),
         pye = lag(ye_act, n = 1, default = 1),
         nys = lead(ys_act,n = 1, default = 0),
         nye  = lead(ye_act, n = 1, default = 0)) %>%
    filter(1:n() != 1 | 1:n() != n()) %>% 
    select(x,ye_act,pye) %>% 
    gather(-x, key = "prob", value= "value") %>% 
    arrange(x, desc(value)) ->final5_a_b
  
 last_event <- res_list[[i]] %>% 
    filter(1:n() == n()) %>% 
    transmute(prob= "1", x=  x, value = ye_act)
  
  test5_ev %>% 
    filter(1:n() == 1 ) %>% 
    select(x,ye_act,pys) %>%   gather(-x, key = "prob", value= "value") %>% 
    arrange(x, desc(value)) %>% 
    bind_rows(final5_a_b) %>% 
    bind_rows(last_event) %>% 
    select(-prob) -> towrite_group1
  dim_j <- dim(towrite_group1)[1] 
  while(dim_j > 0){
    towrite_group1 <- towrite_group1 %>% filter(value - lag(value, default = 0) < 0.005)
    dim_j <- dim_j -dim(towrite_group1)[1] 
  }

  res_list[[i]] <-towrite_group1 %>% filter(value - lag(value, default = 0) < 0.005)
  
  
}

return(res_list)

    
}



