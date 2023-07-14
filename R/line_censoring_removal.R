#' @title Remove Outliers from Color Curves
#' @description This function aids in the removal of outlier values within color curves, which can occur during the analysis of a Kaplan-Meier (KM) study. It works by censoring any y-values greater than the 90th percentile of the intra-group range for each group, thus ensuring a smoother and more interpretable curve.
#' @param dataframe A dataframe with x, y, and group values. Default value is 'step4'.
#' @return A dataframe for each curve, stored in 's4_processed'.
#' @examples
#' # line_censoring_removal(dataframe)
#' @export
line_censoring_removal <- function(dataframe){
  s4 = dataframe
  processed_curves <- list()  # create a list to store processed curves

  for (i in unique(s4$group)){
    curve_select = s4[s4$group == i,]
    ranges <- aggregate(y ~ x, curve_select, function(x) (range(x)[2] - range(x)[1] ))
    kmeans_result <- kmeans(ranges$y, 2)
    maxid = which.max(kmeans_result$centers)
    minid = which.min(kmeans_result$centers)

    max_regular = quantile(ranges$y[which(kmeans_result$cluster == minid)],0.9)
    censor_x = which(kmeans_result$cluster == maxid)

    threshold <- max_regular  # replace this with your actual threshold
    processed_curve <- dplyr::group_by(curve_select, x) %>%
      dplyr::mutate(min_y_in_group = min(y)) %>%
      dplyr::filter(y <= min_y_in_group + threshold) %>%
      dplyr::select(-min_y_in_group)  # remove the helper column

    processed_curves[[i]] <- processed_curve
  }

  # Combine all data frames in the list into a single data frame
  s4_processed <- dplyr::bind_rows(processed_curves)

  return(s4_processed)
}
