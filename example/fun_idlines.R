# fun_idlines
# https://medium.com/datadriveninvestor/understanding-edge-detection-sobel-operator-2aada303b900
#
#fun_sobel <- function(mat1){
#  dim.mat1 <- dim(mat1)
#  x.sobel <- cbind(c(-1,-2,-1),c(0,0,0),c(1,2,1))
#  y.sobel <- t(x.sobel)
#
#mat.sb <-  matrix(data = 0,nrow = dim.mat1[1]-1,ncol = dim.mat1[2]-1)
#Mat.c  <- mat.x <- mat.y <-  mat.sb
#for( i in 2:(dim.mat1[1]-1)){ # row index
#  for(j in 2:(dim.mat1[2]-1)){ # collum index
#
#    mat.x[i-1,j-1] <- sum(mat1[c(i-1,i,i+1),c(j-1,j,j+1)] * x.sobel)
#    mat.y[i-1,j-1]  <- sum(mat1[c(i-1,i,i+1),c(j-1,j,j+1)] * y.sobel)
#    Mat.c[i-1,j-1] <- mat1[i,j]
#  }
#}
#
#mat.sb <- sqrt(mat.x^2 + mat.y^2)
#
#
#return(list(SB=mat.sb, Col = Mat.c))
#}
#
#
#lst.sob <- fun_sobel(mat1 = res.fun_cleanplot$curve_1$fig.BW_clean)
## sobel filter 0.5
#
#v1 <- as.numeric(lst.sob$SB[lst.sob$SB > 0])
#
#lst.sob$Col[lst.sob$SB < 0.5] <-  1
#lst.sob$SB[lst.sob$SB < 0.5] <-  0
#
#v.int <- lst.sob$SB[,100]
#
#fun_collocmax <- function(v.int){
#l1  <- length(v.int)
#for(i in 1:length(v.int)){
#  i.indx <- c(i-1,i+1)
#  i.indx <- i.indx[i.indx > 0  & i.indx<= l1]
#  if(v.int[i] < max(v.int[i.indx])){
#    v.int[i] <- 0
#
#  }
#
#
#}
#return(v.int)
#
#}
#
#l2<- apply(lst.sob$SB,MARGIN = 2, fun_collocmax)
#
#colSums(l2 > 0)
#
#cbind(fun_collocmax(v.int),v.int,lst.sob$Col[,100])
#
#v.int2 <- fun_collocmax(v.int)
#
#l1.log <-  l1$SB > 0.1
#l1.num <-l1.log
#l1.num[l1.log == T] <- 1
#
#
#SB_mat<- l1$SB
#SB_mat[SB_mat < 0.15] <- 0
#
#SB_mat <- rbind(0,cbind(0,SB_mat,0),0)
#
#for( i in 2:(dim(SB_mat)[1]-1)){
#  for( j in 2:(dim(SB_mat)[2]-1)){
#    if(SB_mat[i,j] != 0  ){
#      if(SB_mat[i,j] < max(as.numeric(SB_mat[c(i-1,i + 1),c(j-1,j + 1)]))){
#        SB_mat[i,j] = 0
#      }
#    }
#
#  }
#}
#
#SB_mat2 <- SB_mat[-c(1,dim(SB_mat)[1]),-c(1,dim(SB_mat)[2])]
#l1$SB
#
#l1.num3 <- l1.num[,1:3]
#ret2 <- l1.num == 1
#
#
#as.data.frame( SB_mat) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(abs(pixel) > 0.1) %>%
#  ggplot(aes(x =X, y= y, color = pixel))+ geom_point()
#SB_mat != l1$SB
#
#
#
#
#cbind(SB_mat[,2] > 0.15,l1$SB[,2] >0.15)
#
#as.data.frame( SB_mat != l1$SB) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(abs(pixel) > 0.1) %>%
#  ggplot(aes(x =X, y= y, color = pixel))+ geom_point()
#firstmat <- res.fun_cleanplot$curve_1$fig.BW_clean
#firstmat
#as.data.frame( res.fun_cleanplot$curve_1$fig.BW_clean) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(abs(pixel) > 0.1) %>%
#  ggplot(aes(x =X, y= y, color = pixel))+ geom_point()
#
#
#
#
#as.data.frame( l1) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(abs(pixel) > 0.1) %>%
#  ggplot(aes(x =X, y= y, color = pixel))+ geom_point()
#
#
#
#for(i in 2:(dim(fig.BW)[1]-1)){
#  for(j in 2:(dim(fig.BW)[2]-1)){
#    matx[i-1,j-1] <-  sum(fig.BW[c(i-1,i,i+1),c(j-1,j,j+1)] * x.sobel)
#    maty[i-1,j-1] <-  sum(fig.BW[c(i-1,i,i+1),c(j-1,j,j+1)] * y.sobel)
#
#
#    l2 <-  l1 +1
#  }
#  l1 <-  l1 +1
#
#}
#
#
#}
#fig.BW <- step3$fig.BW
#
## lets say if we can remove the cross
#
#fig.BW[2:dim(fig.BW)[1],200:dim(fig.BW)[2]] <- 1
#
#
#x.sobel <- cbind(c(-1,-2,-1),c(0,0,0),c(1,2,1))
#y.sobel <- t(x.sobel)
#
#matx <- matrix(nrow = length(2:(dim(fig.BW)[1]-1)),
#               ncol = length(2:(dim(fig.BW)[2]-1)))
#
#
#maty <-  matx
#
#l1 <-  1
#l2 <-  1
#for(i in 2:(dim(fig.BW)[1]-1)){
#  for(j in 2:(dim(fig.BW)[2]-1)){
#    matx[i-1,j-1] <-  sum(fig.BW[c(i-1,i,i+1),c(j-1,j,j+1)] * x.sobel)
#    maty[i-1,j-1] <-  sum(fig.BW[c(i-1,i,i+1),c(j-1,j,j+1)] * y.sobel)
#
#
#    l2 <-  l1 +1
#  }
#  l1 <-  l1 +1
#
#}
#
#t1<- as.data.frame(maty) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(abs(pixel) > 0.1)
#
#t2<- as.data.frame(matx) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(abs(pixel) > 0.1)
#
#bind_rows(t1,t2 )%>%
#  ggplot(aes(x = X, y = y, color = pixel)) + geom_point(size = 0.5)
#
## from wikipedia
## https://en.wikipedia.org/wiki/Sobel_operator
## \mathbf {G} ={\sqrt {{\mathbf {G} _{x}}^{2}+{\mathbf {G} _{y}}^{2}}}
#
#mat_edge <- sqrt(matx^2 + maty^2)
#
#
#as.data.frame(mat_edge) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(abs(pixel) > 0.2) %>%
#  ggplot(aes(x = X, y = y, color = pixel)) + geom_point(size = 0.5)
#
#edgedect <- function(v.1,edge.sen,v.lensen){
#
#v.1num <- as.numeric(v.1 > edge.sen)
#
#
#for( i in 2:(length(v.1num)-v.lensen)){
#  if(v.1num[i] != 1){
#    if(any(v.1num[i-1] == 1 & v.1num[i:(i+v.lensen)] == 1) ){
#      v.1num[i] <- 1
#    }
#  }
#}
#
#
#v.2num <- v.1num
#
#v.3 <- vector(mode = "numeric",length = length(v.2num))
#v.3[v.2num == 1 & lag(v.2num, default = 0) == 0] <- 1
#v.3[v.2num == 1 & lead(v.2num, default = 0) == 0] <-  -1
#v3 <- cumsum(v.3)
#return(v3)
#}
#
#
#ret <- apply(mat_edge, MARGIN = 2, FUN = function(x){edgedect(v.1 = x,edge.sen = 0.2,v.lensen = 10) })
#ret1 <- apply(mat_edge, MARGIN = 1, FUN = function(x){edgedect(v.1 = x,edge.sen = 0.2,v.lensen = 10) })
#
#ret3 <- ret == 1 | t(ret1) == 1
#as.data.frame(ret == 1 | t(ret1) == 1) %>%
#  mutate_all(as.numeric) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  filter(pixel > 0) %>%
#  ggplot(aes(x = X, y = y, color = pixel)) + geom_point(size = 0.5)
#
#
#ret4<- rbind(F,cbind(F,ret == 1,F),F)
#dim(ret4)
#
#hist(fig.BW[ret4])
#
#cbind(mat_edge)
#
#newmat1 <- rbind(0,cbind(0,mat_edge,0),0) > 0.2 & fig.BW < 0.9
#
#
#e1 <- as.data.frame(fig.BW) %>%
#  mutate_all(as.numeric) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "col") %>%
#  mutate(X = as.numeric(str_remove(X, "V")))
#
#as.data.frame(newmat1) %>%
#  mutate_all(as.numeric) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  left_join(e1) %>%
#  filter(pixel > 0) %>%
#  ggplot(aes(x = X, y = y, color = col)) + geom_point(size = 0.5)
#
#
#NewDF<- as.data.frame(newmat1) %>%
#  mutate_all(as.numeric) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  left_join(e1) %>%
#  filter(pixel > 0)
#
#
#NewDF %>%
#  mutate( l1 =  kmeans(.$col, centers = 2)$cluster) %>%
#  ggplot(aes(x = X, y = y, color = as.factor(l1))) + geom_point(size = 0.5) + theme_bw()
#
#l3 <- NewDF %>%
#  mutate( l1 =  kmeans(.$col, centers = 2)$cluster)
#
#
#l5 <- matrix(0, nrow = dim(fig.BW)[1], ncol = dim(fig.BW)[2])
#
#for(i in 1:dim(l3)[1]){
#  l5[l3$y[i],l3$X[i]] <- l3$l1[i]
#
#}
#
#
#
# as.data.frame(l5) %>%
#  mutate_all(as.numeric) %>%
#  mutate(y = 1:n()) %>%
#  gather(-y, key ="X", value = "pixel") %>%
#  mutate(X = as.numeric(str_remove(X, "V"))) %>%
#  left_join(e1) %>%
#  filter(pixel > 1) %>%
#  ggplot(aes(x = X, y = y, color = as.factor(pixel))) + geom_point(size = 0.5) + theme_bw()
#
#
# fig.BW1 <- fig.BW*0.5
#
#
#DF1 <-  as.data.frame(l5) %>%
#   mutate_all(as.numeric) %>%
#   mutate(y = 1:n()) %>%
#   gather(-y, key ="X", value = "pixel") %>%
#   mutate(X = as.numeric(str_remove(X, "V")))
#l6 <- l5 == 2
#l7 <- l5 == 1
#
#l6
#
#
#
#fig.BW1 <- fig.BW*0.5
#
#fig.BW1 <-  1-fig.BW1
#fig.BW1[l6] <- 0
#fig.BW1[l7] <- 0
#fig.BW1 <- fig.BW1[dim(fig.BW1)[1]:1,]
#
#writeJPEG(image = fig.BW1, target = here::here("Curves","Dig1.jpeg"))
#
#
#
