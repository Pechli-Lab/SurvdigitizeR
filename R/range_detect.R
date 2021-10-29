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

#' range_detect
#' detects how X and y pixel values map to time and survival values respectively
#' @param step1_fig  figure array from previous step
#' @param step2_axes axes list from previous step
#' @param x_start what does the x-axis start usually 0
#' @param x_end  what does the x-axis end in
#' @param x_increment what does it go up by (Note: increment between EVERY tick, including minor ones)
#' @param y_start what does the y-axis start
#' @param y_increment what does it go up by (Note: increment between EVERY tick, including minor ones)
#' @param y_end  what does it end in
#' @param y_text_vertical whether the y -axis label text is vertical or horizontal)
#'
#' @return # a list with the Y_0pixel where the y-axis starts, y_increment, X_0pixel and x_increment.
#' @export
#'
#' @examples
range_detect  <- function(step1_fig, step2_axes, x_start, x_end, x_increment,
                          y_start, y_increment, y_end, y_text_vertical){


  require(imager)
  # Creating actual scales
  X_actual <- seq(x_start, x_end, by = x_increment)
  Y_actual <- seq(y_start, y_end, by = y_increment)

  ## creating subsets which are the x-axis labels and the y-axis labels
  fig_bw <- step1_fig[,,3]
  fig_x <-fig_bw[-step2_axes$yaxis,step2_axes$xaxis]
  fig_y <-fig_bw[step2_axes$yaxis,-step2_axes$xaxis]

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
  x_pixels_increment  <-as.numeric(names(which.max(table(diff(loc_of_breaks_x)))))
  x_pixels_increment<- mean(diff1[diff1 >=  median(diff1)*0.95  & diff1 <= median(diff1)/0.95])

  # if nr of ticks found is nr of actual ticks -1, 0 tick is missing, add it
  if(length(loc_of_breaks_x) == length(X_actual)-1){
    loc_of_breaks_x <- c(loc_of_breaks_x[1]-x_pixels_increment, loc_of_breaks_x)
  }
  # know that x's go up by x_increment pixels, know the location of the x_increment need to anchor somehow
  xaxis_cimg <- imager::as.cimg(fig_x[1:loc_y_start,])
  xaxis_gray <- as.matrix(xaxis_cimg)
  x1<- ocr(jpeg::writeJPEG(image =  xaxis_gray),HOCR = T)
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
  X_0pixel <- (length(seq(0, x1.tbl$word, by = x_increment))-1)*x_pixels_increment - x1.tbl$pxl_loc

  if(X_0pixel < 0){
    X_0pixel <- min(loc_of_breaks_x)

  }
  # returning that

  ## Step 2 detect line breaks location

  if(y_text_vertical){
    fig_y <-t(fig_y[,dim(fig_y)[2]:1])
  }

  loc_x_start <- which.max(colSums(fig_y <0.90))-1
  ### Detecting y-axis breaks
  y_breaks_loc <- which(fig_y[,(loc_x_start-1)] < 0.60)
  y_breaks <- diff(y_breaks_loc)


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

  # if nr of ticks found is nr of actual ticks -1, 0 tick is missing, add it
  if(length(y_breaks_act) == length(Y_actual)-1){
    y_breaks_act <- c(y_breaks_act[1]-break_indicator_y, y_breaks_act)
  }

  yaxis_cimg <- as.cimg(fig_y[-c(1:loc_x_start),])
  yaxis_gray <- as.matrix(yaxis_cimg)
  y1<- ocr(jpeg::writeJPEG(image =  t(yaxis_gray)),HOCR = T)
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
      Y_0pixel <- (length(seq(0, y1.tbl$word, by = y_increment))-1)*break_indicator_y - y1.tbl$pxl_loc

    } else{
      Y_0pixel <- y1.tbl$pxl_loc

    }
    break_indicator_y

    if(Y_0pixel > min(y_breaks_loc) | Y_0pixel <0 ){

      if(Y_actual[1]>0){
        Y_0pixel <- min(y_breaks_loc) - Y_actual[1]/ (y_increment/break_indicator_y)
      } else {
        Y_0pixel <-  min(y_breaks_loc)
      }


    }

  } else{

    if(Y_actual[1]>0){
      Y_0pixel <- min(y_breaks_loc) - Y_actual[1]/ (y_increment/break_indicator_y)
    } else {
      Y_0pixel <-  min(y_breaks_loc)
    }


  }

  return(list(Y_0pixel= Y_0pixel,
              y_increment = y_increment/break_indicator_y,
              X_0pixel = X_0pixel,
              x_increment = x_increment/x_pixels_increment
  ))
}


















