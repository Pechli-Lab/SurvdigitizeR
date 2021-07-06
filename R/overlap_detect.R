#' overlap_detect
#' this function detects potential overlap between curves and "fills in the gaps"
#' @param fig.grp: dataframe with x,y values and associated curve for each pixel
#'
#' @return res.df: a dataframe with the detected x,y, group values for all curves
#' @export
#'
#' @examples # overlap_detect(df_colordetect)
overlap_detect <- function(fig.grp, k = 200){

  library(FNN)
  window_min <- 20
  window_max <- 100
  curve_width <- 5

  fig.knn <- fig.grp[order(fig.grp$x, -fig.grp$y),]

  groups <- unique(fig.grp$group)

  # get knn indices and distances
  knn <- get.knn(fig.knn, k)
  knn_groups <- t(apply(knn$nn.index, 1, function(r) fig.knn$group[r]))

  # calculate "well-placed" score for each point using knn similarity
  knn_samegroup <- t(apply(cbind(fig.knn$group, knn_groups), 1,
                           function(r) 2*as.integer(r[2:length(r)]==r[1])-1))
  fig.knn$score <- apply(cbind(knn_samegroup, 1/knn$nn.dist), 1,
                     function(r) sum(r[1:k]*(r[1:2*k]))/k)

  # get curve average scores
  groups <- fig.knn %>%
    group_by(group) %>%
    summarise(scoreMean = mean(score)) %>%
    arrange(desc(scoreMean))

  # determine overlaps and best path for each curve
  res.df <- data.frame()
  for (g in groups$group) {

    # get curve points, start window at top left
    points <- fig.knn[fig.knn$group==g,]
    curve_points <- list()
    x_left <- points$x[1]
    y_top <- points$y[1]
    curve_points[[length(curve_points)+1]] <- c(x_left, y_top, g)
    x_final <- max(points[points$y==min(points$y),]$x)

    # stop at the lowest y of this group
    while (x_left < x_final) {

      # determine window bottom right corner
      x_right <- min(x_right + window_min, x_final)
      while (x_right<x_final && (x_right-x_left) < window_max &&
             sum(points$x==x_right) > curve_width &&
             sum(points$x==x_right) == 0) {
        x_right <- x_right +1
      }
      if(sum(points$x==x_right) != 0){
        y_bottom <- min(points[points$x==x_right,]$y) # get y at right side
      } else {
        y_bottom <- min(points[points$x>x_left & points$x<=x_right,]$y) # get min y in window
      }

      # filter out invalid values above or below window
      window_points <- points[points$x>=x_left & points$x<x_right &
                                points$y>y_bottom & points$y<=y_top,]

      # walk through the points in the window and correct issues
      x <- x_left
      y <- y_top
      while (x < x_right | y > y_bottom) {

        # checking the pixel below and the pixel to the right of current coords
        p_down <- window_points[window_points$x == x & window_points$y == y-1,]
        p_right <- window_points[window_points$x == x+1 & window_points$y == y,]
        if (nrow(p_down) > 0 & nrow(p_right) > 0){
          if(p_down$score > p_right$score){ # both exist, below fits better
            y <- y-1
          } else{ # both exist, right fits better
            x <- x+1
          }
        } else if (nrow(p_down) > 0 & nrow(p_right) == 0){ # only below exists
          y <- y-1
        } else if (nrow(p_down) == 0 & nrow(p_right) > 0){ # only right exists
          x <- x+1
        } else if (x >= x_right){ # reached right edge of window, go below
          y <- y-1
        } else if (y <= y_bottom){ # reached below edge of window, go right
          x <- x+1
        } else { # neither below or right exist, try to guess which is missing
          score_down <- sum(window_points[window_points$x == x & window_points$y < y,]$score) +
            sum(window_points[window_points$x > x & window_points$y == y-1,]$score)
          score_right <- sum(window_points[window_points$x == x+1 & window_points$y < y,]$score) +
            sum(window_points[window_points$x > x & window_points$y == y,]$score)

          if(score_down > score_right){ # below fits better
            y <- y-1
          } else{ # right fits better
            x <- x+1
          }
        }

        # add new coords to list
        curve_points[[length(curve_points)+1]] <- c(x, y, g)
      }

      x_left <- x
      y_top <- y
    }

    # put results in dataframe
    group.df <- as.data.frame(do.call(rbind, curve_points))
    colnames(group.df) = c('x', 'y', 'group')
    res.df <- rbind(res.df, group.df)
  }

  return(res.df)
}


