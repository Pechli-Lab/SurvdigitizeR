step1_fig = step1
step2_axes = step2$axes
x_start = x_start
x_end = x_end
x_increment = x_increment
y_start = y_start
y_end = y_end
y_increment = y_increment
y_text_vertical = y_text_vertical


require(imager)
# Creating actual scales
X_actual <- seq(x_start, x_end, by = x_increment)
Y_actual <- seq(y_start, y_end, by = y_increment)

## creating subsets which are the x-axis labels and the y-axis labels
fig_bw <- step1_fig[,,3]
fig_x <-fig_bw[-step2_axes$yaxis,step2_axes$xaxis]
fig_y <-fig_bw[step2_axes$yaxis,-step2_axes$xaxis]

### Detecting when x-axis row starts
loc_y_start <- which.max(rowSums(fig_x <0.90))-1
### Detecting x-axis breaks
loc_of_breaks_x <-which(fig_x[(loc_y_start-1),] < 0.90)
## removing continous x breaks
loc_of_breaks_x <- loc_of_breaks_x[c(0,diff(loc_of_breaks_x)) != 1]


img = load.image(img_path)

subimg <- imsub(img, y %inr% c(dim(img)[2]-loc_y_start,dim(img)[2])) %>% plot
save.image(subimg,"xaxis.png")

text <- tesseract::ocr_data(here::here("xaxis.png"), engine = eng)


bbox0 <- text[text$word == "0",]$bbox
bbox_vec<- apply(str_split(bbox0, pattern = ",",simplify = T), 2, as.numeric)

mean(c(bbox_vec[1],bbox_vec[3]))
