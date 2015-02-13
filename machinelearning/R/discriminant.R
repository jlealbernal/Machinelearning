
# ----------------------------------------------------------------------
# Creating a linear discriminant function with 2 features and 3 classes
# ----------------------------------------------------------------------
#'Creating a linear discriminant function with 2 features and 3 classes
#'
#' @param class It is the matrix we generate. It must contain 2 features and 3 classes
#' @return It returns the slopes and the intercept of the linear discriminant with 2 features and 3 classes. 
#' @export
#' @examples
#' # we can create a matrix of bivariate normal distributions with three different classes
#' class1<-classfunction(nx1=600, nx2=600, nx3=600, mux1=c(14,10), mux2=c(4,4), mux3=c(12,0.5), sdx1=c(1.3,1.3), sdx2=c(0.9,1), sdx3=c(1.2,1), rhox1=-0.72, rhox2=0.3, rhox3=-0.2, name=c("Rabbit", "Cocodrile", "Fish"))
#' discriminant(class1)$intercept
#' discriminant(class1)$slope



##############################################################################


discriminant<-function(class){
  n<-rep(0,3)
  for(i in 1:3){n[i]<-length(class[class[,4]==i,4])}
  nx1<-dim(class[class[,4]==1,])[1]
  nx2<-dim(class[class[,4]==2,])[1]
  nx3<-dim(class[class[,4]==3,])[1]
  X <- as.matrix(cbind(ind=rep(1, nrow(class)),  class[,1:2]))
  Y<-list(Y12=cbind(target1 = c(rep(1, nx1), rep(0, nx2),rep(0, nx3)), target2 = c(rep(0, nx1), rep(1, nx2),rep(0, nx3))), Y13= cbind(target1 = c(rep(1, nx1), rep(0, nx2),rep(0, nx3)), ttarget3 = c(rep(0, nx1), rep(0, nx2),rep(1, nx3))), Y23= cbind(target2 = c(rep(0, nx1), rep(1, nx2),rep(0, nx3)),target3 = c(rep(0, nx1), rep(0, nx2),rep(1, nx3))))
  beta<-list(solve(t(X)%*%X) %*% t(X) %*% Y[[1]], solve(t(X)%*%X) %*% t(X) %*% Y[[2]], solve(t(X)%*%X) %*% t(X) %*% Y[[3]])
  weights<- list(  (beta[[1]][,1]-beta[[1]][,2])[2:3], (beta[[2]][,1]-beta[[2]][,2])[2:3], (beta[[3]][,1]-beta[[3]][,2])[2:3] )
  bias <- list( (beta[[1]][,1]-beta[[1]][,2])[1], (beta[[2]][,1]-beta[[2]][,2])[1],(beta[[3]][,1]-beta[[3]][,2])[1] )
  slope<-list(-(weights[[1]][1]/weights[[1]][2]),  -(weights[[2]][1]/weights[[2]][2]), -(weights[[3]][1]/weights[[3]][2]) )
  intercept<-list ((-bias[[1]])/weights[[1]][2], (-bias[[2]])/weights[[2]][2], (-bias[[3]])/weights[[3]][2])
  results<-list(slope=slope, intercept=intercept)
  return(results)}