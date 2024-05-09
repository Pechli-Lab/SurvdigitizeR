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
#' @param impute_size Size parameter for imputation. It defines the number of time intervals for imputation.
#' @param line_censoring Logical value indicating if line censoring removal should be attempted. Default value is FALSE.
#' @return A dataframe with columns: 'id', 'times', 'St', and 'curve'.
#' @examples
#' \dontrun{
#' survival_digitize(
#'   img_path = here::here("OS.jpg"),
#'   bg_lightness = 0.1,
#'   attempt_OCR = FALSE,
#'   num_curves = 2,
#'   x_start = 0,
#'   x_end = 10,
#'   x_increment = 1,
#'   y_start = 0,
#'   y_end = 1,
#'   y_increment = 0.2,
#'   y_text_vertical = TRUE
#' )
#' }
#' @export
#'
#'
survival_digitize <- function(img_path,
                               bg_lightness = 0.3,
                               attempt_OCR = F,
                               word_sensitivity = 30,
                               num_curves,
                               censoring = F,
                               x_start,
                               x_end,
                               x_increment,
                               y_start,
                               y_end,
                               y_increment,
                               y_text_vertical,
                               nr_neighbors = 20,
                               enhance = F,
                               impute_size = 0,
                               line_censoring = F){


  # Initialize result list
  results <- list()

  # Helper function to handle errors
  handle_error <- function(e, step) {
    stop(sprintf("Error in %s: %s", step, e$message))
  }

  # Step 1: Load curves from file
  results$step1 <- tryCatch({
    img_read(path = img_path)
  }, error = function(e) handle_error(e, "Step 1: Loading image"))

  # Step 2: Identify plot axes location
  results$step2 <- tryCatch({
    axes_identify(fig.hsl = results$step1, bg_lightness = bg_lightness)
  }, error = function(e) handle_error(e, "Step 2: Identifying axes"))
  fig.cropped <- results$step2$fig.hsl
  axes <- results$step2$axes

  # Step 3: Remove plot background pixels and words
  results$step3 <- tryCatch({
    fig_clean(fig.hsl = fig.cropped, bg_lightness = bg_lightness, attempt_OCR = attempt_OCR, word_sensitivity = word_sensitivity)
  }, error = function(e) handle_error(e, "Step 3: Cleaning figure"))

  # Step 4: Group pixels to curves based on color
  results$step4 <- tryCatch({
    color_cluster(fig.df = results$step3, num_curves = num_curves, censoring = censoring, enhance = enhance)
  }, error = function(e) handle_error(e, "Step 4: Clustering colors"))

  # Additional steps if conditions met
  if (line_censoring) {
    results$step4 <- tryCatch({
      line_censoring_removal(results$step4)
    }, error = function(e) handle_error(e, "Line Censoring Removal"))
  }
  if (impute_size > 0) {
    results$step4 <- tryCatch({
      impute_overlap(results$step4, impute_size)
    }, error = function(e) handle_error(e, "Impute Overlap"))
  }

  # Step 5: Fill in missing/overlapped pixels
  results$step5 <- tryCatch({
    overlap_detect(fig.grp = results$step4, nr_neighbors = nr_neighbors)
  }, error = function(e) handle_error(e, "Step 5: Detecting overlaps"))

  # Step 6: Isolate the singular y values for each curve
  results$step6 <- tryCatch({
    lines_isolate(fig.curves = results$step5)
  }, error = function(e) handle_error(e, "Step 6: Isolating lines"))

  # Step 7: Detects how x and y pixels map to time and survival values respectively
  results$step7 <- tryCatch({
    range_detect(step1_fig = results$step1, step2_axes = axes,
                 x_start = x_start, x_end = x_end, x_increment = x_increment,
                 y_start = y_start, y_end = y_end, y_increment = y_increment,
                 y_text_vertical = y_text_vertical)
  }, error = function(e) handle_error(e, "Step 7: Detecting ranges"))

  # Step 8: Summarize figure into data frame
  results$step8 <- tryCatch({
    fig_summarize(lines_vector = results$step6, range_list = results$step7,
                  x_start = x_start, y_start = y_start, y_end = y_end)
  }, error = function(e) handle_error(e, "Step 8: Summarizing data"))

  # Return final results
  return(results$step8)
}
