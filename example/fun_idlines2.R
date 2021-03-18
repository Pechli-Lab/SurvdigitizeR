# fun_idlines2
fun_idlines2 <- function(step3.lst){
  #  Input
      #  Step3.lst
      # num_col 
  #  depends on:
      # imager
      # dplyr
      # tidyr

im_bw  <- suppressWarnings(as.cimg(step3.lst$fig.BW_clean))
im_rbg <- suppressWarnings(as.cimg(step3.lst$fig.arr_clean))

# changing orrientation so x and y axis make sense when loaded into dplyr
im_bw <- mirror(imrotate(im_bw ,angle = 90,),axis = "x")
im_rbg <- mirror(imrotate(im_rbg ,angle = 90,),axis = "x")


 
# Will do a pixel flood on a grid of white to identify line location
matgr <- expand.grid(y =seq(1,dim(im_bw)[1], by = round(dim(im_bw)[1]/10)),
                     x= seq(1,dim(im_bw)[2], by = round(dim(im_bw)[2]/10)))

initial.mat <- matrix(data = F,nrow = dim(im_bw)[1],ncol = dim(im_bw)[2])
for( i in 1:dim(matgr)[1]){
  if(im_bw[matgr$y[i],matgr$x[i],,]  > 0.90){
    initial.mat  <-  (initial.mat |  px.flood(im_bw,x = matgr$x[i],y = matgr$y[i] )[,,1,1])
  }
}

# for each point in im_bw[matgr] , check if it's white if white pixel flood to identify all other white points
for( i in 1:dim(matgr)[1]){
  if(im_bw[matgr$y[i],matgr$x[i],,]  > 0.90){
    initial.mat  <-  (initial.mat |  px.flood(im_bw,x = matgr$x[i],y = matgr$y[i] )[,,1,1])
  }
}

pxf1 <- px.flood(im_bw,x = matgr$x[i],y = matgr$y[i] )
pxf1[,,1,1] <- initial.mat
pxf2 <- !pxf1

ret <- list(step3.lst[[1]],step3.lst[[2]]  , lineloc = pxf2)
names(ret)[1:2] <- names(step3.lst)
return(ret)
}









