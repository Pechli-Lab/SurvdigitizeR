#' survival_digitize a wrapper for the digitizer end-to-end process
#'
#' @param img_path :  location of image file
#' @param bg_lightness : a lightness threshold value between 0 and 1; every pixel with lightness > bg_lightness is considered background and removed (default: 0.9)
#' @param Wsen: word sensitivity for OCR word removal
#' @param OCR_words: logical indicating if needed to remove words
#' @param num_curves: the number of curves in an image
#' @param censoring: locgical indicating if censoring is indicated by black marks
#' @param x_start: lowest X-axis label value
#' @param x_end: highest X-axis label value
#' @param x_increment: what increments do the ticks go up in the x-axis (Note: increment between EVERY tick, including minor ones)
#' @param y_start: lowest y-axis label
#' @param y_end: highest y-axis label
#' @param y_increment: what increments do the ticks go up in the y-axis (Note: increment between EVERY tick, including minor ones)
#' @param y_text_vertical: whether the y-axis labels are vertical (TRUE) or horizontal (FALSE)
#'
#' @return a dataframe with id, times, St, curve
#' @export
#'
#' @examples # survival_digitize(img_path = here::here("OS.jpg"), bg_lightness = 0.1, attempt_OCR = F , num_curves = 2, x_start = 0, x_end = 10, x_increment = 1, y_start = 0 , y_end  = 1, y_increment = 0.2, y_text_vertical = T)

survival_digitize <- function(img_path,
                            bg_lightness = 0.1,
                            attempt_OCR = F,
                            word_sensitivity = 30,
                            num_curves,
                            censoring,
                            x_start,
                            x_end,
                            x_increment,
                            y_start,
                            y_end,
                            y_increment,
                            y_text_vertical){


  # Step 1 load curves from file
  step1 <- img_read(path = img_path)

  # Step 2 identify plot axes location
  step2 <- axes_identify(fig.hsl = step1,bg_lightness = bg_lightness)
  fig.cropped <- step2$fig.hsl
  axes <- step2$axes

  # Step 3 remove plot background pixels and words
  step3 <- fig_clean(fig.hsl = fig.cropped,bg_lightness = bg_lightness, attempt_OCR = attempt_OCR, word_sensitivity = word_sensitivity)

  # Step 4 Group pixels to curves based on color
  step4 <- color_cluster(fig.df = step3, num_curves = num_curves, censoring = censoring)

  # Step 5 Fill in missing/overlapped pixels
  step5 <- overlap_detect(fig.grp =step4 )

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
  step8 <- fig_summarize(lines_vector = step6, range_list =step7)

  return(step8)
}


