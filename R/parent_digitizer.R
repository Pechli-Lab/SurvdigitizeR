#' Parent_digitizer a wrapper for all the digitizer functions
#'
#' @param curve_location : curve location
#' @param sen : a value between 0 and 1 indicating how close a value has to be to white to be considered part of the background
#' @param Wsen_i: word sensitivity for OCR word removal
#' @param OCR_words_i: logical indicating if needed to remove words
#' @param num_curves1: the number of curves in an image
#' @param censoring_i: locgical indicating if censoring is indicated by black marks
#' @param x_start_i: lowest X-axis label value
#' @param x_end_i: highest X-axis label value
#' @param x_incr: what increments do the ticks go up in the x-axis
#' @param y_start_i: lowest y-axis label
#' @param y_end_i: highest y-axis label
#' @param y_incr: what increments do the ticks go up in the y-axis
#' @param Y_values_vertical: whether the y-axis labels are vertical (TRUE) or horizontal (FALSE)
#'
#' @return a dataframe with id, times, St, curve
#' @export
#'
#' @examples # out1<-parent_digitizer(curve_location = here::here("OS.jpg"), sen = 0.1, Wsen_i = 30 , OCR_words_i = F , num_curves1 = 2, x_start_i = 0, x_end_i = 10, x_incr = 1, y_start_i = 0 , y_end_i  = 1, y_incr = 0.2, Y_values_vertical = T)

parent_digitizer <- function(curve_location,
                            sen,
                            Wsen_i,
                            OCR_words_i,
                            num_curves1,
                            x_start_i,
                            x_end_i,
                            x_incr,
                            y_start_i,
                            y_end_i,
                            y_incr,
                            Y_values_vertical,
                            censoring_i){


  # Step 1 read in curve
  step1 <-fun_readsurv(FilePath = curve_location)
  # Step 2 id plot location
  step2 <- fun_idplot(fig.hsl = step1,i.sen = sen)
  step3 <-fun_cleanplot(fig.hsl = step2$fig.hsl,i.sen = sen, Wsen = Wsen_i,OCR_words = OCR_words_i )
  step3 <- step3 %>%
    filter(l >= .1)
  step4 <-fun_colordetect(fig.df = step3, num_curves = num_curves1, black_marks = censoring_i)
  step5 <- overlap_detect(fig.grp =step4 )
  step6 <-eventdetect(res.df = step5)
  step7 <-get_tofinal(res_list = step6)
  fig.BW <- step1[,,3]
  step8 <- fun_range(X_start =x_start_i,
                     X_end = x_end_i,
                     X_increment = x_incr,
                     Y_end = y_end_i,
                     Y_start = y_start_i,
                     Y_increment = y_incr,
                     step1_bw =fig.BW,step2_axis = step2$axis,
                     Y_values_vertical = T)
  step9 <- fun_summary(final_list = step7,Step7_out =step8)

  return(step9)
}


