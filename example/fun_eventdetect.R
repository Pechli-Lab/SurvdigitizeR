getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fun_eventdetect <- function(step7.df, cen = T,cenclose= 2){
 
  
 # step7.df <-l7
 #cen = T
 #cenclose = 2

  
  num_gr <- unique(step7.df$group)
  num_curves  <- length(unique(step7.df$group))
  res.df <- vector(length = num_curves, mode = "list")
  if(cen == T){

    for(k1 in 1:num_curves){
      temp_df <- step7.df[step7.df$group == num_gr[k1],]
      temp_df1 <- temp_df %>% 
        mutate(segment = 1:n())
      
      
      cycle.i <- 1  
      retry.i <- 1
      
      while (T) {
        n.gri <- unique(temp_df1$segment)
        l.gr1 = length(n.gri)
       # message(paste("\r","interation",cycle.i, "num segments", l.gr1 ))
        cycle.i <- cycle.i + 1 # update cycle
        
        for(j in n.gri){
          mem.curr.group <- which(temp_df1$segment == j)
          if(length(mem.curr.group > 0)){ # make sure that there are some members of current group
            left.x <- mem.curr.group[which.max(temp_df1$x[mem.curr.group])]
            loc.leftx <-  temp_df1$x[left.x]
            r1 <-  temp_df1$segment != j & temp_df1$x == loc.leftx + 1 & abs(temp_df1$ye - temp_df1$ye[left.x]) <= 1 & abs(temp_df1$ys - temp_df1$ys[left.x] )<= 1
            if(any(r1)){
              segrep <- temp_df1$segment[r1] 
              temp_df1$segment[temp_df1$segment == segrep] <- j
            }
            
            
          }
        }
        
        # Control structure 
        if(length(unique(temp_df1$segment))== l.gr1 ){ 
          retry.i <- retry.i + 1
          if(retry.i > 2){
            break
          }
        } else {
          retry.i <-  1
        }
        
        if(cycle.i == 1000){break} # emergency break
        
      }
      
      
      temp_df2 <- temp_df1 %>% 
        group_by(segment) %>% 
        summarise(xmin = min(x),
                  xmax = max(x),
                  ymin = min(c(ys,ye)),
                  ymax = max(c(ys,ye))) %>% ungroup()
      
      temp_df3 <- temp_df2 %>% 
        mutate(drop = ymax-ymin,
               length = xmax-xmin)
      
      # Finding all boxes 
      
      temp_df3 <- temp_df3 %>% 
        filter(drop != 0)
      
      
      temp_df3 <- temp_df3 %>% 
        mutate( modedrop = getmode(abs(temp_df1$ys-temp_df1$ye))) %>% 
        filter(abs(drop - modedrop) > cenclose) 
      
      temp_df4 <- temp_df3 %>% 
        mutate(cen = drop == min(drop)) %>% 
        mutate(Status = as.numeric(!cen)) %>% 
        mutate( strata =num_gr[k1] )
      
      res.df[[k1]] <- temp_df4
      
    }
    
  }
  
  
  # what if cen is equal to F
  
  if(cen == F){
    
    for(k1 in 1:num_curves){
      temp_df <- step7.df[step7.df$group == num_gr[k1],]
      temp_df1 <- temp_df %>% 
        mutate(segment = 1:n())
      
      
      cycle.i <- 1  
      retry.i <- 1
      
      for( i in 1:1000){
        n.gri <- unique(temp_df1$segment)
        l.gr1 = length(n.gri)
        # message(paste("\r","interation",cycle.i, "num segments", l.gr1 ))
        cycle.i <- cycle.i + 1 # update cycle
        
        for(j in n.gri){
          mem.curr.group <- which(temp_df1$segment == j)
          if(length(mem.curr.group > 0)){ # make sure that there are some members of current group
            left.x <- mem.curr.group[which.max(temp_df1$x[mem.curr.group])]
            loc.leftx <-  temp_df1$x[left.x]
            r1 <-  temp_df1$segment != j & temp_df1$x == loc.leftx + 1 & abs(temp_df1$ye - temp_df1$ye[left.x]) <= 1 & abs(temp_df1$ys - temp_df1$ys[left.x] )<= 1
            if(any(r1)){
              segrep <- temp_df1$segment[r1] 
              temp_df1$segment[temp_df1$segment == segrep] <- j
            }
            
            
          }
        }
        
        # Control structure 
        if(length(unique(temp_df1$segment))== l.gr1 ){ 
          retry.i <- retry.i + 1
          if(retry.i > 2){
            break
          }
        } else {
          retry.i <-  1
        }
        
        if(cycle.i == 1000){break} # emergency break
        
      }
      
      
      temp_df2 <- temp_df1 %>% 
        group_by(segment) %>% 
        summarise(xmin = min(x),
                  xmax = max(x),
                  ymin = min(c(ys,ye)),
                  ymax = max(c(ys,ye))) %>% ungroup()
      
      temp_df3 <- temp_df2 %>% 
        mutate(drop = ymax-ymin,
               length = xmax-xmin)
      
      # Finding all boxes 
      
      temp_df3 <- temp_df3 %>% 
        filter(drop != 0)
      
      
      temp_df3 <- temp_df3 %>% 
        mutate( modedrop = getmode(abs(temp_df1$ys-temp_df1$ye))) %>% 
        filter(abs(drop - modedrop) > cenclose) 
      
      temp_df3 <- temp_df3 %>% 
        mutate(event = 1) %>% 
        mutate(strata =k1)
      
      res.df[[k1]] <-temp_df3 
      
    }
    
  }
  
  
  retdf<- do.call(rbind,res.df)
  
  return(retdf)
  
}


#
#
#
#
# temp_df <- step7.df[step7.df$group == num_gr[k1],]
# temp_df1 <- temp_df %>% 
#   mutate(segment = 1:n())
# 
# 
# cycle.i <- 1  
# retry.i <- 1
# 
# for( i in 1:100){
#   n.gri <- unique(temp_df1$segment)
#   l.gr1 = length(n.gri)
#   message(paste("\r","interation",cycle.i, "num segments", l.gr1 ))
#   cycle.i <- cycle.i + 1 # update cycle
#   
#   for(j in n.gri){
#     mem.curr.group <- which(temp_df1$segment == j)
#     if(length(mem.curr.group > 0)){ # make sure that there are some members of current group
#       left.x <- mem.curr.group[which.max(temp_df1$x[mem.curr.group])]
#       loc.leftx <-  temp_df1$x[left.x]
#       r1 <-  temp_df1$segment != j & temp_df1$x == loc.leftx + 1 & abs(temp_df1$ye - temp_df1$ye[left.x]) <= 1 & abs(temp_df1$ys - temp_df1$ys[left.x] )<= 1
#       if(any(r1)){
#         temp_df1$segment[r1] <- j
#       }
#       
#       
#     }
#   }
#   
#   # Control structure 
#   if(length(unique(temp_df1$segment))== l.gr1 ){ 
#     retry.i <- retry.i + 1
#     if(retry.i > 2){
#       break
#     }
#   } else {
#     retry.i <-  1
#   }
#   
#   if(cycle.i == 1000){break} # emergency break
#   
# }
# 
# 
# temp_df2 <- temp_df1 %>% 
#   group_by(segment) %>% 
#   summarise(xmin = min(x),
#             xmax = max(x),
#             ymin = min(c(ys,ye)),
#             ymax = max(c(ys,ye))) %>% ungroup()
# 
# temp_df3 <- temp_df2 %>% 
#   mutate(drop = ymax-ymin,
#          length = xmax-xmin)
# 
# # Finding all boxes 
# 
# temp_df3 <- temp_df3 %>% 
#   filter(drop != 0)
# 
# 
# temp_df3 <- temp_df3 %>% 
#   mutate( modedrop = getmode(abs(temp_df1$ys-temp_df1$ye))) %>% 
#   filter(abs(drop - modedrop) > cenclose) 
# 
# temp_df4 <- temp_df3 %>% 
#   mutate(cen = drop == min(drop)) %>% 
#   mutate(Status = as.numeric(!cen))
# 
# res.df[[k1]] <- temp_df4
# 
#

#



# Same while loop and control structure


#ycle.i <- 1  
#etry.i <- 1
# 
#or( i in 1:1000){
#   n.gri <- unique(temp_df1$segment)
#   l.gr1 = length(n.gri)
#   message(paste("\r","interation",cycle.i, "num segments", l.gr1 ))
#   cycle.i <- cycle.i + 1 # update cycle
#   
#or(j in n.gri){
# mem.curr.group <- which(temp_df1$segment == j)
# if(length(mem.curr.group > 0)){ # make sure that there are some members of current group
#   left.x <- mem.curr.group[which.max(temp_df1$x[mem.curr.group])]
#   loc.leftx <-  temp_df1$x[left.x]
#    r1 <-  temp_df1$segment != j & temp_df1$x == loc.leftx + 1 & abs(temp_df1$ye - temp_df1$ye[left.x]) <= 1 & abs(temp_df1$ys - temp_df1$ys[left.x] )<= 1
#  if(any(r1)){
#    temp_df1$segment[r1] <- j
#  }

# 
# }
#

#   # Control structure 
#   if(length(unique(temp_df1$segment))== l.gr1 ){ 
#     retry.i <- retry.i + 1
#     if(retry.i > 2){
#       break
#     }
#   } else {
#     retry.i <-  1
#   }
#   
#   if(cycle.i == 1000){break} # emergency break
#   
#
# 

#emp_df2 <- temp_df1 %>% 
#   group_by(segment) %>% 
#   summarise(xmin = min(x),
#             xmax = max(x),
#             ymin = min(c(ys,ye)),
#             ymax = max(c(ys,ye))) %>% ungroup()

#emp_df3 <- temp_df2 %>% 
# mutate(drop = ymax-ymin,
#        length = xmax-xmin)

# # Finding all boxes 
# 
#emp_df3 <- temp_df3 %>% 
# filter(drop != 0)
# 
# 
#emp_df3 <- temp_df3 %>% 
# mutate( modedrop = getmode(abs(temp_df1$ys-temp_df1$ye))) %>% 
# filter(abs(drop - modedrop) > cenclose) 
#
#emp_df4 <- temp_df3 %>% 
# mutate(cen = drop == min(drop)) %>% 
# mutate(Status = as.numeric(!cen))



# # here need to create algorithm
# for(i in 1:10){
#   v.gr <- unique(line_df2$gr)
#   n.gr <- length(v.gr)
#   message(paste("\r","interation",c1, "current segments ", n.gr ))
#   c1 <- c1+ 1
#   
#   for(g in v.gr){
#     l.g   <- which(line_df2$gr == g)
#     l.g <- l.g[length(l.g)]
#     
#     if(length(l.g)> 0){
#       l.x<- which(line_df$x == line_df$x[l.g] + 1)
#       if(length(l.x)> 0){
#         l.j<- l.x[which((line_df2$ye[l.x] %in%  c(line_df2$ye[l.g],line_df$ye[l.g] +1 ,line_df2$ye[l.g]-1)   )
#                         &line_df2$ys[l.x] %in% c(line_df2$ys[l.g],line_df$ys[l.g] +1 ,line_df2$ys[l.g] -1))]
#         if(length(l.j) >0)
#           line_df2$gr[l.j] <- g
#       }
#       
#     }
#     
#   }
#   
#   
#   if(length(unique(line_df$gr)) == n.gr ){ 
#     r1 <- r1 + 1
#     if(r1 > 2){
#       break
#     }
#   } else {
#     r1 <-  1
#   }
# }
# 
# 
# line_df2 %>% ggplot(aes(x,ymin= ye, ymax = ys,color = as.factor(gr))) +  geom_linerange() + 
#   guides(color = F)
# 
# 
# line_df3 <- line_df2 %>% group_by(gr) %>% 
#   summarise(xmin = min(x),
#             xmax = max(x),
#             ymin = min(c(ye,ys)),
#             ymax = max(c(ye,ys))) %>% ungroup()
# 
# 
# line_df4 <- line_df3 %>% 
#   mutate(drop = ymax - ymin) %>% 
#   filter(drop %in%  mod) %>% 
#   mutate(mode = min(drop)) %>% 
#   mutate(cen = case_when(drop == mode ~ 0, T ~ 1))
# 
# 
# 
# if(F){
#   line_df2 %>% count(lr, dr) %>% arrange(desc(lr))
#   # need a way to group
#   line_df2 <- line_df %>% mutate(gr2 = gr) %>% arrange(desc(lr))
#   
#   line_df3 <- line_df2 %>% group_by(gr) %>% 
#     summarise(xmin = min(x),
#               xmax = max(x),
#               ymin = min(c(ye,ys)),
#               ymax = max(c(ye,ys))) %>% ungroup()
#   
#   line_df2 %>% arrange(x) %>% 
#     filter(x %in% 230:250) %>% print(n = 21)
#   
#   
#   
#   line_df3 <- line_df3 %>% 
#     mutate(gr = as.numeric(as.factor(gr)))
#   
#   line_df4<- line_df3 %>% 
#     mutate(l1 = xmax-xmin,
#            drop = ymax - ymin) 
#   
#   line_df4 %>% 
#     arrange(xmin)
#   
#   
#   line_df4 %>% arrange(desc(drop))
#   
#   
#   line_df4 %>% arrange(xmax) %>% mutate(ysn = lag(ymax),
#                                         yse = lag(ymin),
#                                         ysp = lead(ymax),
#                                         yse = lead(ymin)) %>% 
#     ggplot(aes(ymin= ymin, ymax = ymax, xmin = xmin -1, xmax=xmax + 1 ,fill = as.factor(gr))) + geom_rect() + 
#     guides(fill = F)
#   
#   
#   
#   
#   ord1  <- table(unlist(line_df4$l1))[order(table(unlist(line_df4$l1)),decreasing = T)]
#   loc1 <- as.numeric(names(ord1))
#   loc1 <- loc1[!(loc1  %in% c(loc1[1] +1, loc1[1] -1))]
#   
#   line_df3 %>% 
#     mutate(dr = ymax-ymin) %>% 
#     filter(!(dr %in% c(loc1[1], loc1[1] + 1, loc1[1] - 1))) %>% 
#     filter(dr != 0) %>% 
#     ggplot(aes(ymin= ymin, ymax = ymax, xmin = xmin, xmax=xmax ,fill = as.factor(gr))) + geom_rect() + 
#     guides(fill = F)
#   
#   
#   c1 <-  1
#   r1 <- 0
#   
#   for(i in 1:200){
#     l.gr3 <- unique(line_df3$gr2)
#     n_gr3 <- length(l.gr3)
#     message(paste("\r","interation",c1, "current group", n_gr3 ,"tries", r1))
#     c1 <-  c1 +1
#     for(i in l.gr3){
#       l.gr <- which(line_df3$gr2 == i)
#       if(length(l.gr) > 0){
#         l.left <- which(line_df3$xmax[l.gr] == max(line_df3$xmax[l.gr]))
#         if(length(l.left)>1) l.left <-l.left[1] 
#         l.x  <- line_df3$xmax[l.gr[l.left]] + 1
#         p.gr <- which(line_df3$xmin %in% line_df3$xmax[l.left]:line_df3$xmin[l.left])
#         for(j in p.gr){
#           if(any(line_df3$ymin[j]:line_df3$ymax[j] %in% line_df3$ymin[l.left]:line_df3$ymax[ l.left])){
#             line_df3$gr2[j] <- i
#           }
#         }
#       }
#     }  
#     
#     
#     
#     if(length(unique(line_df3$gr2)) == n_gr3 ){ 
#       r1 <- r1 + 1
#       if(r1 == 2){
#         break } 
#     } else {
#       r1 <- 1
#     }
#   }
#   
#   line_df3
#   
#   line_df3 %>% select(gr,gr2) %>%  right_join(select(line_df2,-gr2)) %>% 
#     ggplot(aes(x , ymin = ys, ymax = ye, color =as.factor(gr2)))+ geom_linerange() + guides(color = F)
#   
#   
#   
#   # triyng to create a new grouping
#   
#   line_df3 <- line_df2 %>% group_by(gr) %>% 
#     summarise(xmin = min(x),
#               xmax = max(x),
#               ymin = min(c(ye,ys)),
#               ymax = max(c(ye,ys))) %>% ungroup() %>% mutate(gr2 = as.numeric(as.factor(gr)))
#   
#   
#   line_df3
#   
#   
#   ### HERE----
#   
#   line_df1 <- line_df %>% mutate(gr = as.numeric(as.factor(gr)))
#   
#   line_df2 <- line_df1 %>% group_by(gr) %>% 
#     summarise(xmin = min(x),
#               xmax = max(x), 
#               ymin = min(c(ye,ys)),
#               ymax = max(c(ye,ys))) %>% ungroup()
#   
#   line_df2 <- line_df2 %>% mutate(gr1 = gr)
#   
#   
#   
#   for(i in 1:num_col){
#     list.col[[i]]<- im_rgb_filt2 %>% 
#       filter(kmean == i ) %>% dplyr::select(-kmean) %>% 
#       gather(`1`,`2`,`3`, key = "cc", value = "value" ) %>% 
#       mutate(cc =  as.numeric(cc))
#     
#   }
#   # returning to as.cimg w/ the goal of id'ing vertical line segments
#   p1 <- as.cimg(list.col[[1]]) != 0
#   
#   mat1 <- p1[,,1,1]
#   mat1 <-  t(mat1)
#   
#   # dimensions of mat1
#   d.mat1 <- dim(mat1)
#   n <-  1
#   
#   # this loop goes through every vector and identifies any continues line segments
#   res.v.end <- list()
#   res.v.start<- list()
#   res.v.j<- list()
#   res.v.group<- list()
#   
#   for(j in 1:d.mat1[2]){ # collumn wise
#     v.1 <- mat1[,j] # column vector
#     v.end <-  which(lead(v.1,1,0) == 0 & v.1 == 1)  # start and stop of line segments
#     v.start <- which(lag(v.1,1,0) == 0 & v.1 == 1)  
#     if(length(v.end) > 0 & length(v.start) == length(v.end)){
#       for( i in seq_along(v.end)){
#         res.v.j[[n]] <- j
#         res.v.end[[n]] <- v.end[i]
#         res.v.start[[n]] <- v.start[i]
#         res.v.group[[n]] <- n
#         
#         mat1[v.start[i]:v.end[i],j] <- n 
#         n <- n +1
#       }
#     }
#   }
#   
#   
#   
#   # creating a a dataframe of line range
#   
#   line_df <- data.frame( x= unlist(res.v.j),
#                          ye = unlist(res.v.end),
#                          ys = unlist(res.v.start),
#                          gr = unlist(res.v.group))
#   
#   
#   line_df %>% ggplot(aes(x,ymin= ye, ymax = ys,color = as.factor(gr))) +  geom_linerange() + 
#     guides(color = F)
#   r1 <-  1
#   c1 <- 1
#   while(T){
#     v.gr <- unique(line_df$gr)
#     n.gr <- length(v.gr)
#     message(paste("\r","interation",c1, "current segments ", n.gr ))
#     c1 <- c1+ 1
#     
#     for(g in v.gr){
#       l.g   <- which(line_df$gr == g)
#       l.g <- l.g[length(l.g)]
#       
#       if(length(l.g)> 0){
#         l.x<- which(line_df$x == line_df$x[l.g] + 1)
#         if(length(l.x)> 0){
#           l.j<- l.x[which((line_df$ye[l.x] %in%  c(line_df$ye[l.g],line_df$ye[l.g] +1 ,line_df$ye[l.g]-1)   )
#                           &line_df$ys[l.x] %in% c(line_df$ys[l.g],line_df$ys[l.g] +1 ,line_df$ys[l.g] -1))]
#           if(length(l.j) >0)
#             line_df$gr[l.j] <- g
#         }
#         
#       }
#       
#     }
#     
#     if(length(unique(line_df$gr)) == n.gr ){ 
#       r1 <- r1 + 1
#       if(r1 == 3){
#         break
#       }
#     } else {
#       r1 <-  1
#     }
#     
#     
#     
#     pxlf <- step4.list$lineloc
#     im_rgb <- as.cimg(step4.list$fig.arr_clean)
#     im_rgb <- mirror(imrotate(im_rgb ,angle = 90,),axis = "x")
#     
#     im_rbg_df <- as.data.frame(im_rgb ,wide = "c")
#     im_rbg_df_flt <-  as.data.frame(pxlf) %>% 
#       dplyr::select(-cc,-z) %>% left_join(im_rbg_df)
#     
#     im_rbg_df_flt %>% 
#       ggplot(aes(x,y)) + geom_raster()
#     
#     num_col <- 2 
#     im_rbg_df_flt2  <-  im_rbg_df_flt %>% 
#       mutate(gr = kmeans(cbind(c.1,c.2,c.3),centers = num_col)$cluster)
#     
#     list_col <- vector(length = num_col, mode = "list")
#     for(i in 1:num_col){
#       im_rbg_df1 <- im_rbg_df %>% mutate(c.1 =1,c.2= 1,c.3=1)
#       list_col[[i]] <- im_rbg_df1 %>% 
#         bind_rows(im_rbg_df_flt2[im_rbg_df_flt2$gr == i,]) %>% group_by(x,y) %>% 
#         mutate(n = n()) %>% ungroup() %>% filter(!( n > 1 & is.na(gr))) %>% select(-gr,-n) %>% 
#         gather(starts_with("c."), key = "cc", value = "value") %>% 
#         mutate(cc = as.numeric(str_remove(cc,"c."))) %>% as.cimg()
#       
#     }
#     
#     
#     
#   }
#   
#   
#   
#   line_df %>% ggplot(aes(x,ymin= ye, ymax = ys,color = as.factor(gr))) +  geom_linerange() + 
#     guides(color = F)
#   
#   line_df %>% group_by(gr) %>% 
#     summarise(xmin = min(x),
#               xmax = max(x),
#               ymin = min(ye),
#               ymax = max(ys)) 
#   
#   l3<- line_df %>% 
#     group_by(gr) %>% 
#     summarise(xmin = min(x), xmax = max(x), ymin = min(ye), ymax = max(ys)) %>% ungroup() %>% 
#     mutate(dr = ymax-ymin) %>% 
#     mutate(dn = xmax-xmin) %>% 
#     group_by(gr) %>% 
#     mutate(list1 = list(rep(dr, times = dn)))
#   
#   l3 %>% 
#     ggplot(aes(xmin=  xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = as.factor(dr))) + geom_rect()
#   
#   frq_dr <- as.numeric(names(table(l3$dr)[order(table(l3$dr),decreasing = T)] ))
#   cen <-  T 
#   
#   l3 %>% ungroup() %>% arrange(xmax) %>% mutate(ny = lag(ymax)) %>%  
#     filter(!(dr %in% c(frq_dr[1],frq_dr[1] +1 ,frq_dr[1]-2))  & dr != 0) %>% 
#     ggplot(aes(xmin=  xmin, ymin = ymin, xmax = xmax, ymax = ymax, fill = as.factor(dr))) + geom_rect()
#   
#   # here is where i am 
#   # need to turn this into censoring and need to functionalize
#   
# }
#
#}