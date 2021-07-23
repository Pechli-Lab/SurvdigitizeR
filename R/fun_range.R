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

#' fun_range
#' detects how X and y pixel values map to time and survival values respectively
#' @param X_start what does the x-axis start usually 0
#' @param X_end  what does the x-axis end in
#' @param X_increment what does it go up by
#' @param Y_start what does the y-axis start
#' @param Y_increment what does it go up by
#' @param Y_end  what does it end in
#' @param step1_bw  from previous step
#' @param step2_axis from previous step
#' @param Y_values_vertical whether the y -axis labels are vertical or horizontal: does this make sense)
#'
#' @return # a list with the Y_0pixel where the y-axis starts, y_increment, X_0pixel and X_increment.
#' @export
#'
#' @examples
fun_range  <- function(X_start, X_end,X_increment,Y_start,Y_increment,Y_end,step1_bw,step2_axis,Y_values_vertical){
  ## function starts here
require(imager)
  # Creating actual scales
  X_actual <- seq(X_start, X_end, by = X_increment)
  Y_actual <- seq(Y_start, Y_end, by = Y_increment)

  ## creating subsets which are the x-axis labels and the y-axis labels
  fig_x <-step1_bw[-step2_axis$yaxis,step2_axis$xaxis]
  fig_y <-step1_bw[step2_axis$yaxis,-step2_axis$xaxis]

  ### Detecting when x-axis row starts
  loc_y_start <- which.max(rowSums(fig_x <0.90))-1
  ### Detecting x-axis breaks
  loc_of_breaks_x <-which(fig_x[(loc_y_start-1),] < 0.90)
  ## removing continous x breaks
  loc_of_breaks_x <- loc_of_breaks_x[c(0,diff(loc_of_breaks_x)) != 1]
  diff1 <-diff(loc_of_breaks_x)
  # fun_ggplot(fig_x[1:loc_y_start,]) +
  # geom_vline(xintercept = loc_of_breaks_x)

  # getting the increase in x-breaks
  x_increment  <-as.numeric(names(which.max(table(diff(loc_of_breaks_x)))))
  x_increment<- mean(diff1[diff1 >=  median(diff1)*0.95  & diff1 <= median(diff1)/0.95])
  # know that x's go up by x_increment pixels, know the location of the x_increment need to anchor somehow
  xaxis_cimg <- imager::as.cimg(fig_x[1:loc_y_start,])
  xaxis_gray <-   grayscale( xaxis_cimg)
  x1<- ocr(jpeg::writeJPEG(image =  xaxis_gray[,,1,1]),HOCR = T)
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

# # allow for only one match
#   if(length( loc_of_breaks_x[!is.infinite(match_loc)]) != length(  x1.tbl$pxl_loc) ){
#   match_loc[duplicated(match_loc ) & is.finite(match_loc)] <- Inf
#   }

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
  y_breaks_loc <- which(fig_y[-c(1:loc_x_start),][1,] < 0.60)
  y_breaks <- diff(which(fig_y[-c(1:loc_x_start),][1,] < 0.60))


  gr1 <- cumsum(!c(1,y_breaks) == 1)

  y_breaks_act <- vector(length = length(unique(gr1)))

  for(i in seq_along(unique(gr1))){


    y_breaks_act[i] <- round(mean(y_breaks_loc[gr1 == unique(gr1)[i]]))
  }


  y_breaks_loc <- y_breaks_act
  y_breaks <- y_breaks[y_breaks!= 1]


  ## detect breaks

  break_indicator_y <-as.numeric(names(table(diff(y_breaks_act))))[which.max(table(diff(y_breaks_act)))]
  break_indicator_y <- round(break_indicator_y)
  yaxis_cimg <- as.cimg(fig_y[-c(1:loc_x_start),])
  yaxis_gray <-   grayscale( yaxis_cimg)
  y1<- ocr(jpeg::writeJPEG(image =  yaxis_gray[,,1,1]),HOCR = T)
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
    break_indicator_y

    if(Y_0pixel > min(y_breaks_loc) | Y_0pixel <0 ){
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


















