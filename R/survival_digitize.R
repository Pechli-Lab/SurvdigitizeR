#' @title Survival Digitization
#' @description A wrapper for the digitizer end-to-end process.
#' @param img_path Location of the image file.
#' @param bg_lightness A lightness threshold value between 0 and 1. Every pixel with lightness > bg_lightness is considered background and removed. Default value is 0.3.
#' @param attempt_OCR Logical value indicating if Optical Character Recognition (OCR) should be attempted to remove words. Default value is FALSE.
#' @param word_sensitivity Sensitivity for OCR word removal. Default value is 30.
#' @param num_curves The number of curves in an image.
#' @param censoring Logical value indicating if censoring is represented by black marks. Default value is TRUE.
#' @param x_start Lowest X-axis label value.
#' @param x_end Highest X-axis label value.
#' @param x_increment Increment value for the X-axis ticks. Note: this should include increment between every tick, including minor ones.
#' @param y_start Lowest Y-axis label.
#' @param y_end Highest Y-axis label.
#' @param y_increment Increment value for the Y-axis ticks. Note: this should include increment between every tick, including minor ones.
#' @param y_text_vertical Boolean value indicating if the Y-axis labels are vertical (TRUE) or horizontal (FALSE).
#' @param nr_neighbors Number of neighbors in k-nearest neighbors (knn) when grouping pixels by colors. Default value is 50.
#' @param enhance Logical value indicating whether to convert HSL channels into the same scale. Default value is FALSE.
#' @param impute_size Size parameter for imputation.
#' @return A dataframe with columns: 'id', 'times', 'St', and 'curve'.
#' @examples
#' # survival_digitize(img_path = here::here("OS.jpg"), bg_lightness = 0.1, attempt_OCR = FALSE, num_curves = 2, x_start = 0, x_end = 10, x_increment = 1, y_start = 0, y_end = 1, y_increment = 0.2, y_text_vertical = TRUE)
#' @export
#'
#'
survival_digitize <- function(img_path,
                               bg_lightness = 0.3,
                               attempt_OCR = F,
                               word_sensitivity = 30,
                               num_curves,
                               censoring = T,
                               x_start,
                               x_end,
                               x_increment,
                               y_start,
                               y_end,
                               y_increment,
                               y_text_vertical,
                               nr_neighbors = 50,
                               enhance = F,
                               impute_size = 0){


  # Step 1 load curves from file
  step1 <- img_read(path = img_path)

  # Step 2 identify plot axes location
  step2 <- axes_identify(fig.hsl = step1,bg_lightness = bg_lightness)
  fig.cropped <- step2$fig.hsl
  axes <- step2$axes

  # Step 3 remove plot background pixels and words
  step3 <- fig_clean(fig.hsl = fig.cropped,bg_lightness = bg_lightness, attempt_OCR = attempt_OCR, word_sensitivity = word_sensitivity)

  # Step 4 Group pixels to curves based on color
  step4 <- color_cluster(fig.df = step3, num_curves = num_curves, censoring = censoring,  enhance = enhance)

  if(impute_size > 0){
  step4 <- impute_overlap(step4,impute_size)}
  # Step 5 Fill in missing/overlapped pixels
  step5 <- overlap_detect(fig.grp =step4, nr_neighbors =nr_neighbors) #add num_neighbor as parameter

  # Step 6 isolate the singular y values for each curve
  step6 <-lines_isolate(fig.curves = step5)

  # Step 7 detects how x and y pixels map to time and survival values respectively
  step7 <- range_detect(step1_fig = step1,
                         step2_axes = step2$axes,
                         x_start = x_start,
                         x_end = x_end,
                         x_increment = x_increment,
                         y_start = y_start,
                         y_end = y_end,
                         y_increment = y_increment,
                         y_text_vertical = y_text_vertical)



  # Step 8 detects how x and y pixels map to time and survival values respectively
  step8 <- fig_summarize(lines_vector = step6, range_list =step7,y_start = y_start,y_end = y_end)

  return(step8)
}


