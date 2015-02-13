require(mvtnorm)

# ----------------------------------------------------------------------
# Creating a  bivariate normal distribution
# ----------------------------------------------------------------------
#'
#'Creating a  bivariate normal distribution
#'
#'
#' @param nx1 Number of observations from the class/group 1.
#' @param nx2 Number of observations from the class/group 2.
#' @param nx3 Number of observations from the class/group 3.
#' @param mux1 Vector of two means (c(mean1, mean2)) from each feature from the class/group 1.
#' @param mux2 Vector of two means (c(mean1, mean2)) from each feature from the class/group 2.
#' @param mux3 Vector of two means (c(mean1, mean2)) from each feature from the class/group 3.
#' @param sdx1 Vector of two standard deviations (c(sdx1, sdx2)) from each feature from the class/group 1.
#' @param sdx2 Vector of two standard deviations (c(sdx1, sdx2)) from each feature from the class/group 2.
#' @param sdx3 Vector of two standard deviations (c(sdx1, sdx2)) from each feature from the class/group 3.
#' @param rhox1 Correlation coefficient of the two features in class 1.
#' @param rhox2 Correlation coefficient of the two features in class 2.
#' @param rhox3 Correlation coefficient of the two features in class 3.
#' @param name: By default, since we are dealing with a three class function, it assigns Dog, Cat and Rabbits name. We can specify it.
#' @return It returns a data frame with the first two columns being the observations from each feature normally distributed, the third column states the group those observations belong to and the last column assigns 1, 2 and 3 to each group.
#' @export
#' @import mvtnorm 
#' @examples
#' # we can create a matrix of bivariate normal distributions with three different classes
#' class<-classfunction(nx1=200, nx2=200, nx3=200, mux1=c(5,6), mux2=c(1,6), mux3=c(3,4), sdx1=c(1.3,1.3), sdx2=c(0.9,1), sdx3=c(1.2,1),rhox1=-0.72, rhox2=0.3, rhox3=-0.2, name=c("Cocodrile", "Rabbit", "Fish") )
#' plot(class$Var1, class$Var2, col=class$Label, xlab="Feature 1", ylab="Feature 2", main="Features by groups distribution")
#' 


classfunction <- function(nx1, nx2, nx3, mux1, mux2, mux3, sdx1, sdx2, sdx3, rhox1, rhox2, rhox3, name) {
  #covariance matrix per each group
  sigmax1 <- matrix(c( (sdx1[1])^2, rhox1,rhox1, (sdx1[2])^2), 2, 2, byrow = TRUE)
  sigmax2 <- matrix(c( (sdx2[1])^2, rhox2,rhox2, (sdx2[2])^2), 2, 2, byrow = TRUE)
  sigmax3 <- matrix(c( (sdx3[1])^2, rhox3,rhox3, (sdx3[2])^2), 2, 2, byrow = TRUE)
    #generation of bivariate normal distribution per each class
  class1 <- rmvnorm(nx1, mux1, sigmax1)
  class2 <- rmvnorm(nx2, mux2, sigmax2)
  class3 <- rmvnorm(nx3, mux3, sigmax3)
  #joining all
  class <- as.data.frame(rbind(class1,class2, class3))
  label<- c(rep(name[1], nx1), rep(name[2], nx2), rep(name[3], nx3))
  class <- cbind(class, label)
  class<-cbind(class,rbind(matrix(1,nx1,1),matrix(2,nx2,1),matrix(3,nx3,1)))
  colnames(class) <- c("Var1", "Var2", "Label", "Group")
  return(class)
}

