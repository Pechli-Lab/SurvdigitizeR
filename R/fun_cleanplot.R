#' fun_cleanplot
#' Removes color values close to white, also possible to remove text on plot, useful for gridlines on KM curves
#' @param fig.hsl array of pixels in HSL format (hue/saturation/lightness)
#' @param i.sen how close a value is to white to consider it part of the background
#' @param Wsen Word sensitivity for image recognition
#' @param OCR_words a logical indicating whether to attempt text recognition
#'
#' @return fig.df: a dataframe with x,y and h,s,l values for each remaining pixel
#' @export
#'
#' @examples # fun_cleanplot(figlst, i.sen = 0.2, Wsen = 30, OCR_words = F)
fun_cleanplot  <- function(fig.hsl, i.sen, Wsen, OCR_words){

  require(tesseract)
  require(stringr)
  require(dplyr)
  require(xml2)



  if(OCR_words == T){

    # flip arrays for OCR
    fig.l <- fig.hsl[,,3]
    fig.l_flip <- fig.l[dim(fig.l)[1]:1,]
    fig.hsl_flip <- fig.hsl[dim(fig.hsl)[1]:1,,]

    # Taken from a stack exchange basically takes xml output into a tibble
    k1<- ocr( writeJPEG(fig.l_flip),HOCR = T)
    doc <- read_xml(k1)
    nodes <- xml_find_all(doc, ".//span[@class='ocrx_word']")
    words <- xml_text(nodes)
    meta <- xml_attr(nodes, 'title')
    bbox <- str_replace(str_extract(meta, "bbox [\\d ]+"), "bbox ", "")
    conf <- as.numeric(str_replace(str_extract(meta, "x_wconf.*"), "x_wconf ", ""))

    if(any(conf > Wsen)){
      res_ocr <- tibble(confidence = conf, word = words, bbox = bbox)

      bbox_mat <- apply(str_split(res_ocr$bbox, pattern = " ",simplify = T), 2, as.numeric)
      bbox_mat <- as.data.frame(bbox_mat)

      colnames(bbox_mat) <- c("x0","y0","x1","y1")

      res_ocr <- bind_cols(res_ocr,bbox_mat)
      res_ocr <- filter(res_ocr, confidence > Wsen)


      # Removing identified words
      for(i in 1:dim(res_ocr)[1] ){
        fig.hsl_flip[res_ocr$y0[i]:res_ocr$y1[i],res_ocr$x0[i]:res_ocr$x1[i],] <- c(0,0,1)
      }
    }

    # flip array back
    fig.hsl <- fig.hsl_flip[dim(fig.hsl_flip )[1]:1, ,]
  }

  # removing based on white sensitivity
  fig.l <- fig.hsl[,,3]
  non_bg.mat <- which(1 - fig.l > i.sen, arr.ind=T)
  non_bg_h <- fig.hsl[cbind(non_bg.mat, rep(1, dim(non_bg.mat)[1]))]
  non_bg_s <- fig.hsl[cbind(non_bg.mat, rep(2, dim(non_bg.mat)[1]))]
  non_bg_l <- fig.hsl[cbind(non_bg.mat, rep(3, dim(non_bg.mat)[1]))]

  # convert remaining points to dataframe
  fig.df <- data.frame(non_bg.mat, as.numeric(non_bg_h), non_bg_s, non_bg_l)
  colnames(fig.df) <- c("y", "x", "h", "s", "l")

  return(fig.df)
}


