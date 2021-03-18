#' clean_plot
#'
#' Cleans plot output from name of function
#' @param step2lst
#' @param i.sen Sensitivity parameter indicating how close a value is to one to when we consider it a 0
#' @param Wsen Sensitivity for removing words in a figure
#' @param OCR_words Logical indicating whether to try and remove text on a graph
#'
#' @return list with cleaned plot and black and white array
#' @export
#' @examples
#' # clean_plot(step25lst = step25,i.sen = 0.1,Wsen = 30)
clean_plot  <- function(step25lst, i.sen, Wsen, OCR_words){
# Input:
  # step25lst: a list from fun_readsurv.R with only the plot that's been subsetted to include only the plot
  # i.sen: how close a value is to one where we consider it a 0
  # W.sen: sensitivity for image recognition
  # OCR_words: a logical indicating wether to attempt text recognition
# Output:
  # fig.rgbclean: an array from removing pixels that are not relevant
# Dependency:

# To do
  # to do specify hue as 0
  # be able to remove + and |
  # check for other curves
  fig.BW_flip <- step25lst$fig.BW[dim(step25lst$fig.BW)[1]:1,]
  fig.arr_flip <- step25lst$fig.arr[dim(step25lst$fig.arr)[1]:1,,]



  if(OCR_words == T){
  # Taken from a stack exchange basically takes xml output into a tibble
  k1<- ocr( writeJPEG(fig.BW_flip),HOCR = T)
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



    # Removing identified colors
    for(i in 1:dim(res_ocr)[1] ){
      fig.BW_flip[res_ocr$y0[i]:res_ocr$y1[i],res_ocr$x0[i]:res_ocr$x1[i]] <- 1
      # Don't know how to turn hue into 0
      fig.arr_flip[res_ocr$y0[i]:res_ocr$y1[i],res_ocr$x0[i]:res_ocr$x1[i],] <- 1


    }

  }
  }
  # removing based on white sensitivity
  white.mat <- 1-fig.BW_flip < i.sen
  white.mat.ar <-  array(dim = c(dim(white.mat),3 ))
  white.mat.ar[,,1] <- white.mat.ar[,,2] <-  white.mat.ar[,,3] <-  white.mat


  fig.BW_flip[white.mat] <- 1
  fig.arr_flip[white.mat.ar] <- 1

  fig.BW_flip <- fig.BW_flip[dim(fig.BW_flip)[1]:1, ]
  fig.arr_flip <- fig.arr_flip [dim(fig.arr_flip )[1]:1, ,]

return(list(fig.BW_clean = fig.BW_flip,
            fig.arr_clean = fig.arr_flip))

}


