
fun_idcolor2 <- function(step4.list, num_col){
    pxlf <- step4.list$lineloc
  im_rgb <-suppressWarnings( as.cimg(step4.list$fig.arr_clean))
  im_rgb <- mirror(imrotate(im_rgb ,angle = 90,),axis = "x")
  
  im_rbg_df <- as.data.frame(im_rgb ,wide = "c")
  im_rbg_df_flt <-  as.data.frame(pxlf) %>% 
    dplyr::select(-cc,-z) %>% left_join(im_rbg_df, by = c("x","y"))

  im_rbg_df_flt2  <-  im_rbg_df_flt %>% 
    mutate(gr = kmeans(cbind(c.1,c.2,c.3),centers = num_col)$cluster)
  
  list_col <- vector(length = num_col, mode = "list")
  for(i in 1:num_col){
    
    suppressWarnings({
    im_rbg_df1 <- im_rbg_df %>% mutate(c.1 =1,c.2= 1,c.3=1)
    list_col[[i]] <- im_rbg_df1 %>% 
      bind_rows(im_rbg_df_flt2[im_rbg_df_flt2$gr == i,]) %>% group_by(x,y) %>% 
      mutate(n = n()) %>% ungroup() %>% filter(!( n > 1 & is.na(gr))) %>% select(-gr,-n) %>% 
      gather(starts_with("c."), key = "cc", value = "value") %>% 
      mutate(cc = as.numeric(str_remove(cc,"c."))) %>% as.cimg( )
    
    })
    
  }
  
  grs <-im_rbg_df_flt2 %>% group_by(gr) %>% summarise_all(mean)
  
  

return(list(fig = step4.list, col_fig = list_col, grs = grs ))
}




