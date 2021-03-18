# find range and curves

# curves from step1
fun_ocrtotbl <- function(k1){
  doc <- read_xml(k1)
  nodes <- xml_find_all(doc, ".//span[@class='ocrx_word']")
  words <- xml_text(nodes)
  meta <- xml_attr(nodes, 'title')
  bbox <- str_replace(str_extract(meta, "bbox [\\d ]+"), "bbox ", "")
  conf <- as.numeric(str_replace(str_extract(meta, "x_wconf.*"), "x_wconf ", ""))
  res_ocr <- tibble(confidence = conf, word = words, bbox = bbox)
  bbox_mat <- apply(str_split(res_ocr$bbox, pattern = " ",simplify = T), 2, as.numeric)
  bbox_mat <- as.data.frame(bbox_mat)
  colnames(bbox_mat) <- c("x0","y0","x1","y1")
  res_ocr <- bind_cols(res_ocr,bbox_mat)
  return(res_ocr)
}

fun_rangedast <- function(res.lst,res.axis, Yaxis_horz = T, conf1 ){
#  fun_curve2<- fun_rangedast(res.lst = res.fun_readsurv$curve_4, res.axis = res.fun_idplot$curve_4$axi,Yaxis_horz = T)

W_sen.x <- 40 

# OCR for X 
xaxis_cimg <- as.cimg(res.lst$fig.arr[min(res.axis$yaxis):1,,])
xaxis_gray <-   grayscale( xaxis_cimg)
x1<- ocr(writeJPEG(image =  xaxis_gray[,,1,1]),HOCR = T)
x1.tbl <-  fun_ocrtotbl(x1)

x1.tbl2 <- x1.tbl %>% 
  mutate(num = as.numeric(word)) %>% 
  na.omit() %>% 
  filter(confidence > conf1)
  

x1.tbl3 <- x1.tbl2 %>% arrange(x0) %>% 
  mutate(dif = num - lag(num)) %>% na.omit() %>% 
  mutate(pxl = 0)

for(i in 1:dim(x1.tbl3)[1]){
  temp1 <- xaxis_gray[,x1.tbl3$x0[i]:x1.tbl3$x1[i],1,1] < 0.90
  d.temp1 <- dim(temp1)
  daststartsat <- max(which(rowSums(temp1) >  dim(temp1)[2]*0.9))+ 1
  
  x1.tbl3$pxl[i]  <- (x1.tbl3$x0[i]:x1.tbl3$x1[i])[median(which(temp1[daststartsat,] == T))]
  
  
}

x1.tbl4 <- x1.tbl3 %>% 
  mutate(difpxl = pxl - lag(pxl ))  %>% na.omit() 


x1.tbl5 <- x1.tbl4 %>% 
  mutate(actloc = pxl  - min(res.axis$xaxis))


# OCR for Y 

yaxis_cimg <- as.cimg(res.lst$fig.arr[,min(res.axis$xaxis):1,])
yaxis_gray <-   grayscale(yaxis_cimg)


if(Yaxis_horz){
  y1 <-  ocr(writeJPEG(image = t(yaxis_gray[1:dim(yaxis_gray)[1],dim(yaxis_gray)[2]:1,1,1])), HOCR = T)
  y1.tbl<- fun_ocrtotbl(y1)
  y1.tbl2 <- y1.tbl %>% 
    mutate(num = as.numeric(word)) %>% 
    na.omit() %>% 
    filter(confidence >  conf1)
  
  y1.tbl3<-   y1.tbl2 %>% 
    mutate(y1 = dim(yaxis_gray)[1] -y1 ,
           y0 = dim(yaxis_gray)[1] -y0) %>% 
    arrange(y0) %>% 
    mutate(dif = num - lag(num)) %>% na.omit() %>% 
    mutate(pxl = 0)

   
  for(i in 1:dim(y1.tbl3)[1]){
    temp1 <- yaxis_gray[y1.tbl3$y0[i]:y1.tbl3$y1[i],,1,1] < 0.90
    d.temp1 <- dim(temp1)
    daststartsat <- max(which(colSums(temp1) >  dim(temp1)[1]*0.9))+ 1
    
    y1.tbl3$pxl[i]  <- (y1.tbl3$y0[i]:y1.tbl3$y1[i])[median(which(temp1[,daststartsat] == T))]
    
    
  }
  return(list(yocr =y1.tbl3, xocr = x1.tbl3))
  
}
return(list( xocr = x1.tbl3))


}
   
fun_Range2  <- function(X_start, X_end,X_increment,Y_start,Y_increment,Y_end,step1_bw,step2_axis,Y_values_vertical){
  
  # X_start: what does the x-axis start in 
  # X_end: what does the x-axis end it
  # X_increment: what does it go up by 
  # Y_start: what does the y-axis start
  # Y_increment: what does it go up by
  # Y_end: what does it end in
  # step1_bw,step2_axis: from previou steps
  # whether the y -axis labels are vertical or horizontal: does this make sense)
  
  ## function starts here 
  
  # Creating actual scales
  X_actual <- seq(X_start, X_end, by = X_increment)
  Y_actual <- seq(Y_start, Y_end, by = Y_increment)
  
  ## creating subsets which are the x-axis labels and the y-axis labels
  fig_x <-step1_bw[-step2_axis$yaxis,step2_axis$xaxis]
  fig_y <-step1_bw[step2_axis$yaxis,-step2_axis$xaxis]
  
  ### Detecting when x-axis row starts
  loc_y_start <- which.max(rowSums(fig_x <0.90))-1
  ### Detecting x-axis breaks
  loc_of_breaks_x <-which(fig_x[(loc_y_start-1),] < 0.95)
  ## removing continous x breaks
  loc_of_breaks_x <- loc_of_breaks_x[c(0,diff(loc_of_breaks_x)) != 1]
  
  # fun_ggplot(fig_x[1:loc_y_start,]) + 
  # geom_vline(xintercept = loc_of_breaks_x)
  
  # getting the increase in x-breaks 
  
  x_increment  <-as.numeric(names(which.max(table(diff(loc_of_breaks_x)))))
  
  # know that x's go up by x_increment pixels, know the location of the x_increment need to anchor somehow
  xaxis_cimg <- as.cimg(fig_x[1:loc_y_start,])
  xaxis_gray <-   grayscale( xaxis_cimg)
  x1<- ocr(writeJPEG(image =  xaxis_gray[,,1,1]),HOCR = T)
  x1.tbl <-  fun_ocrtotbl(x1)
  
  ## Which of my words where properly detected by OCR
  
  x1.tbl <-x1.tbl[as.numeric(x1.tbl$word) %in% X_actual,]
  
  # Try and match the bounding box to an index in loc_of_breaks_x
  
  match_loc  <- unlist( lapply(loc_of_breaks_x, function(x){
    
    r1 <- which(x >= x1.tbl$x0 & x <= x1.tbl$x1)
    if(length(r1) == 0 ){
      r1 <- Inf
    }
    return(r1)
  }))
  
  x1.tbl$pxl_loc <- loc_of_breaks_x[!is.infinite(match_loc)]
  x1.tbl <-x1.tbl [which.max(x1.tbl$confidence ),]
  
  ## The start
  X_0pixel <- (length(seq(0, x1.tbl$word, by = X_increment))-1)*x_increment - x1.tbl$pxl_loc
  
  if(X_0pixel < 0){
    X_0pixel <- min(loc_of_breaks_x)
    
  }
  # returning that 
  
  ## Step 2 detect line breaks location 
  
  if(Y_values_vertical){
    fig_y <-t(fig_y[,dim(fig_y)[2]:1])  
  }
  
  loc_x_start <- max(which(rowSums(fig_y < 0.95) == max(rowSums(fig_y < 0.95))))
  y_breaks_loc <- which(fig_y[-c(1:loc_x_start),][1,] < 0.85)
  y_breaks <- diff(which(fig_y[-c(1:loc_x_start),][1,] < 0.85))
  y_breaks_loc <- y_breaks_loc[c(0,y_breaks)  != 1]
  y_breaks <- y_breaks[y_breaks!= 1]

  
  ## detect breaks 
  
  break_indicator_y <-as.numeric(names(table(y_breaks)))[which.max(table(y_breaks))]
  
  yaxis_cimg <- as.cimg(fig_y[-c(1:loc_x_start),])
  yaxis_gray <-   grayscale( yaxis_cimg)
  y1<- ocr(writeJPEG(image =  yaxis_gray[,,1,1]),HOCR = T)
  y1.tbl <-  fun_ocrtotbl(y1)
  ## TODO Fix this 
  y1.tbl <-y1.tbl[as.numeric(y1.tbl$word) %in% Y_actual,]
  # need to find zero anchorings
  
  if(dim(y1.tbl) != 0){
  
  match_loc_y  <- unlist( lapply(y_breaks_loc, function(x){
    
    r1 <- which(x >= y1.tbl$x0 & x <= y1.tbl$x1)
    if(length(r1) == 0 ){
      r1 <- Inf
    }
    return(r1)
  }))
  
  y1.tbl$pxl_loc <- y_breaks_loc[!is.infinite(match_loc_y)]
  y1.tbl <-y1.tbl [which.max(y1.tbl$confidence ),]
  
  ## The start
  if( y1.tbl$word != 0){
    Y_0pixel <- (length(seq(0, y1.tbl$word, by = Y_increment))-1)*break_indicator_y - y1.tbl$pxl_loc
    
  } else{
    Y_0pixel <- y1.tbl$pxl_loc
    
  }
  Y_0pixel
  break_indicator_y
  
  if(Y_0pixel < 0 ){
    Y_0pixel <-  min(y_breaks_loc)
    
    
  }
  
  } else{
    
    
    Y_0pixel <-  min(y_breaks_loc)
    
  }
  
  return(list(Y_0pixel= Y_0pixel,
              y_increment = Y_increment/break_indicator_y, 
              X_0pixel = X_0pixel,
              x_increment = X_increment/x_increment
  ))
}



  













