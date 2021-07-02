#' fun_colordetect
#' detects the location of each curve in the image using medoids
#' @param fig.df a dataframe with x,y and h,s,l values for each pixel
#' @param num_curves the number of curves that are on the figure to be digitized
#' @param black_marks  logical indicating whether censoring occurs in a diff color
#'
#' @return fig.grp: a dataframe with x,y,h,s,l values and associated curve for each pixel
#' @export
#'
#' @examples # fun_colordetect(fig.df =  fig.df, num_curves = 3)
fun_colordetect_old <- function(fig.df, num_curves = NULL, black_marks = NULL){

  # exact number of color clusters required was given
  if (!is.null(num_curves) && !is.null(black_marks)) {
    centers <- num_curves + if(black_marks) 1 else 0
    explore <- F

    # number of curves known but existence of marks unknown
  } else if (!is.null(num_curves)) {

    cand_params <- matrix(c(num_curves, num_curves+1, 0, 1), nrow=2, ncol=2)
    explore <- T

    # unknown number of curves but known existence of black censor marks
  } else if (!is.null(black_marks)) {

    cand_curves <- 2:4
    cand_params <- cbind(cand_curves, rep(as.numeric(black_marks),
                                          length(cand_curves)))
    explore <- T

    # unknown number of curves and existence of black censor marks
  } else {

    cand_curves <- 2:4
    no_mark_removal <- cbind(cand_curves, rep(0, length(cand_curves)))
    mark_removal <- cbind(cand_curves[2:length(cand_curves)],
                          rep(1, length(cand_curves)-1))
    cand_params <- rbind(no_mark_removal, mark_removal)
    explore <- T
  }

  require("gdata", warn.conflicts=FALSE)
  library("cluster")
  library(data.table)


  #' diag_connected
  #' finds points that are well connected, biased on the diagonal x=-y
  #' @param data_df data df with x,y positions
  #' @param k number of nearest neighbors to compare with (default: 20)
  #'
  #' @return df of points with knn score (higher is better)
  #' @export
  diag_knn <- function(data_df, k = 20) {

    # sample <- data_df[sample(nrow(data_df), 20), c('x', 'y', 'group')]

    # order by x,y, get k previous and next
    comp_df <- data_df[order(data_df$x,data_df$y),]
    df_slices <- list()
    for (i in 1:k) {
      temp_df <- comp_df
      temp_df$x_n <- lag(comp_df$x, n = i)
      temp_df$y_n <- lag(comp_df$y, n = i)
      temp_df$group_n <- lag(comp_df$group, n = i)
      df_slices[[i]] <- drop_na(temp_df)
    }
    for (i in 1:k) {
      temp_df <- comp_df
      temp_df$x_n <- lead(comp_df$x, n = i)
      temp_df$y_n <- lead(comp_df$y, n = i)
      temp_df$group_n <- lead(comp_df$group, n = i)
      df_slices[[k+i]] <- drop_na(temp_df)
    }
    comp_df <- data.frame(rbindlist(df_slices))

    # calculate distances, only keep <k> closest
    comp_df$dist_group <- as.numeric(!comp_df$group==comp_df$group_n)
    comp_df$dist_eucl <- sqrt((comp_df$x-comp_df$x_n)^2 + (comp_df$y-comp_df$y_n)^2)
    comp_df$dist_diag <- abs(comp_df$x-comp_df$x_n + comp_df$y-comp_df$y_n)/sqrt(2)
    comp_df$dist_both <- comp_df$dist_eucl + 2*comp_df$dist_diag
    comp_df <- comp_df %>%
      arrange(dist_both) %>%
      group_by(x,y) %>% slice(1:k)

    comp_df$dist_norm <- (comp_df$dist_both-min(comp_df$dist_both))/
      (max(comp_df$dist_both)-min(comp_df$dist_both))
    # comp_df$dist_norm <- scale(comp_df$dist_both, center = F)

    # calculate connection metric
    comp_df$metric <- abs(comp_df$dist_group)
    knn_df <- comp_df %>% group_by(x,y,group) %>% summarise(knn = mean(comp_df$dist_both),
                                                            .groups = "drop")
    return(knn_df)
  }

  #' get_clusters
  #' performs medoid clustering and filters bg and black censor marks
  #' @param centers nr of cluster centers
  #' @param remove_black whether to find and remove black censor marks
  #'
  #' @return list[df, bl_score] results dataframe, nr black censor marks removed
  #' @export
  get_clusters <- function(centers, remove_black = F) {

    # running cluster algorithm to group into colours based on number of curves

    # kmeans
    # out1 <- kmeans(x = comp1[,-c(1:2)], centers = centers, nstart=3)
    # centerpoints <- out1$centers[,-4]
    # sizes <- out1$size
    # cluster_data <- out1$cluster

    # increase the effect of the value component
    in1 <- comp1[,c('h','s','v')]
    in1$v <- in1$v^2
    in1$h <- in1$h^2

    # medoids
    out1 <- clara (x = in1, sampsize = 500, k = centers,
                   stand = T,samples = 50)
    centerpoints <- out1$medoids
    sizes <- out1$clusinfo[,'size']
    cluster_data <- out1$clustering

    # find the most common color in the plot, that's the background
    gr_white <- which.max(sizes)

    # put into df, remove background color
    cand_df <- data.frame(y = comp1$y, x = comp1$x, v=comp1$v, group = cluster_data)
    cand_df <-cand_df %>%
      filter(group != gr_white)

    # if black censor marks specified, remove darkest group
    # evaluate clustering step based on nr of removed and color darkness
    bl_score <- 0
    if(remove_black) {
      gr_black <- which.min(centerpoints[,'v'])
      bl_score <- nrow(cand_df[cand_df$group == gr_black,]) *
        min(centerpoints[,'v'])

      # TODO temporary
      # remove censor marks for now
      cand_df <- cand_df %>%
        filter(group != gr_black)
    }

    return(list("df" = cand_df, "bl_score" = bl_score))
  }

  #' eval_clusters
  #' evaluates the quality of clustering by the distances between points
  #' @param cand_df clustered points dataframe
  #'
  #' @return list[sd_score, knn_score] stand. dev and knn eval metrics
  #' @export
  eval_clusters <- function(cand_df) {

    # evaluate clustering based on y-axis deviation metric
    # calculate standard y-axis deviation of same-x values
    sd_df <- as_data_frame(cand_df %>% group_by(x,group) %>%
                             summarise(sd = sd(y), .groups="drop"))
    sd_df$sd[is.na(sd_df$sd)] <- 0
    sd_score <- mean(sd_df$sd)

    # evaluate clustering based on connectivity metric
    # see cran.r-project.org/web/packages/clValid/vignettes/clValid.pdf
    knn_df <- diag_knn(data_df = cand_df[,c("y","x", "group")])

    # only keep well-clustered y values for every x
    # line_df <- as_data_frame(knn_df %>% group_by(x,group) %>%
    #                            filter(knn == max(knn)))

    knn_score <- 1/as.numeric(mean(knn_df$knn^2)^(1/2))

    return(list("sd_score" = sd_score, "knn_score" = knn_score))
  }

  # if all parameters were exactly defined, get result
  if(!explore){
    final_df <- get_clusters(num_curves)$df

    # otherwise look for best configuration
  } else {

    # try all possible curve numbers, with or without black censor marks
    candidates <- list()
    nr_curves <- c()
    sd_scores <- c()
    knn_scores <- c()
    bl_scores <- c()

    # try all parameter combinations
    for (i in 1:nrow(cand_params)) {
      res <- get_clusters(cand_params[i,1], remove_black = as.logical(cand_params[i,2]))
      candidates[[i]] <- res$df
      nr_curves[i] <- cand_params[i,1] - as.logical(cand_params[i,2])
      bl_scores[i] <- res$bl_score
      metrics <- eval_clusters(res$df)
      sd_scores[i] <- metrics$sd_score
      knn_scores[i] <- metrics$knn_score
    }

    # normalize and combine scores, prioritize fewer curves
    # (more groups tend to separate better, giving false positives)
    scores <- as.vector(scale(sd_scores) + scale(knn_scores) +
                          .5*scale(bl_scores)) + .5*nr_curves

    # pick best fit based on aggregated score
    final_df <- candidates[[which.min(scores)]]
  }

  return(final_df)
}

