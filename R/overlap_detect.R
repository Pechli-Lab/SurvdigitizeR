#' overlap_detect
#' detects potential overlap between curves and "fills in the gaps" using knn similarity
#' @param fig.grp: dataframe with x,y values and associated curve for each pixel
#' @param nr_neighbors: how many nearby neighbors to consider when guessing the value of missing pixels (default: 100)
#'
#' @return res.df: a dataframe with the detected x,y, group values for all curves
#' @export
#'
#' @examples # overlap_detect(fig.dataframe, nr_neighbors = 100)
overlap_detect <- function(fig.grp, nr_neighbors = 50){

  library(FNN)
  c <- 5

  fig.knn <- fig.grp[order(fig.grp$x, -fig.grp$y),]
  groups <- unique(fig.grp$group)

  # get knn indices and distances
  knn <- get.knn(fig.knn, nr_neighbors)
  knn_groups <- t(apply(knn$nn.index, 1, function(r) fig.knn$group[r]))

  # calculate "well-placed" score for each point using knn similarity
  knn_samegroup <- t(apply(cbind(fig.knn$group, knn_groups), 1,
                           function(r) 2*as.integer(r[2:length(r)]==r[1])-1))
  fig.knn$score <- apply(cbind(knn_samegroup, 1/knn$nn.dist^2), 1,
                         function(r) sum(r[1:nr_neighbors] * (r[1:2*nr_neighbors]))
                         / nr_neighbors)

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

    # choose between rightmost and lowest x get final x value based on score
    bottom_right_x <- max(points[points$y<=min(points$y)+5,]$x)
    bottom_right_y <- min(points[points$x == bottom_right_x,]$y)
    rightmost_x <- max(points$x)
    rightmost_y <- min(points[points$x == rightmost_x,]$y)
    if (sum(points[points$x>=bottom_right_x-c & points$x<=bottom_right_x+c &
                   points$y>=bottom_right_y-c & points$y<=bottom_right_y+c,]) >
        sum(points[points$x>=rightmost_x-c & points$x<=rightmost_x+c &
                   points$y>=rightmost_y-c & points$y<=rightmost_y+c,])) {
      x_final <- bottom_right_x
    } else {
      x_final <- rightmost_x
    }
    y_final <- min(points$y)


    x <- x_left
    y <- y_top
    # traverse the line and fix issues
    while (x <= x_final && y>=y_final) {

      # checking the pixel below and the pixel to the right of current coords
      p_down <- points[points$x == x & points$y == y-1,]
      p_right <- points[points$x == x+1 & points$y == y,]
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
      } else { # neither below or right exist, try to guess which is missing
        score_down <- sum(points[points$x == x & points$y < y,]$score) +
          sum(points[points$x > x & points$y == y-1,]$score)
        score_right <- sum(points[points$x == x+1 & points$y < y,]$score) +
          sum(points[points$x > x & points$y == y,]$score)

        if(score_down > score_right){ # below fits better
          y <- y-1
        } else{ # right fits better
          x <- x+1
        }
      }

      # add new coords to list
      curve_points[[length(curve_points)+1]] <- c(x, y, g)
    }

    # put results in dataframe
    group.df <- as.data.frame(do.call(rbind, curve_points))
    colnames(group.df) = c('x', 'y', 'group')
    res.df <- rbind(res.df, group.df)
  }

  return(res.df)
}


