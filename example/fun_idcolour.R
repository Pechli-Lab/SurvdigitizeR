#fun_idcolour  <- function(step3list){
## Input:
#  #step3list:  list object created in step3
## Output:
#  # step4list: a matrix where lines are identified based on colours
## Dependency:
#  #
## To do
#  # - Alan can you give this a try?
#
#
#}
#
#
## Example of code that doesnt work very well
#num_col = 4
#col.v <- as.numeric(step3$fig.hsv_clean[,,1])
#res1 <- kmeans(x = col.v, centers = num_col)
## df.plot$colgroup <- res1$centers[res1$cluster]
#
#l1 <- matrix(data = res1$cluster, nrow =  dim(step3$fig.hsv_clean[,,1])[1], ncol =  dim(step3$fig.hsv_clean[,,1])[2])
#
#as.data.frame(l1) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(pixel != 3) %>%
#  ggplot(aes(x = X, y = y, color  = pixel)) + geom_point(size = 0.1)
