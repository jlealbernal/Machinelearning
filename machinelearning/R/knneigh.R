require(dplyr)

# ----------------------------------------------------------------------
# kNN classifier for three  classes in a space of 2 dimensions
# ----------------------------------------------------------------------
#' kNN classifier for three  classes in a space of 2 dimensions
#' 
#' @param data A data frame or a matrix where rows are observations and columns are features. If \code{type} is "train" this is training dataset, and if it is "predict" it is test dataset.
#' @param trueClasses A vector with labels for each row in \code{data}. If \code{type} is "train", and with labels for each row in \code{memory} if \code{type} is "predict".
#' @param memory A data frame or a matrix where rows are observations and columns are features. If \code{type} is "train" this argument  is not needed, and if it is "predict" it is a training dataset.
#' @param k Number of neighbors that the classifier should use. It has to be an odd number.
#' @param p Distance metric the classifieruses: 1, 2 or Inf. 
#' @param type Whether the goal is to train the classifier or predict classes of new observations based on past ones. The value can be either "train" or "predict".
#' @return A list of predictedClasses, prob, accuracy and errorCount.
#' @export
#' @import assertthat 
#' @examples
#' nx1<-200; nx3<-200; nx2<-200
#' class<-classfunction(nx1, nx2, nx3, mux1=c(5,6), mux2=c(1,6), mux3=c(3,4), sdx1=c(1.3,1.3), sdx2=c(0.9,1), sdx3=c(1.2,1), rhox1=-0.72, rhox2=0.3, rhox3=-0.2, name=c("Elephant", "Tiger", "Lion"))
#'k<-20
#'Accuracy<-rep(0,k)
#'for(i in 1:k){ Accuracy[i]<-knneigh(class[,1:2], class[,4], k=i, p=2, type="train")$accuracy}
#'plot(Accuracy)
#' 
#' 
#' 

##############################################################################

knneigh <- function(data, trueClasses=NULL, memory=NULL, k=1, p=2, type="train") {
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  } 
  
library(assertthat)
  not_empty(data); not_empty(trueClasses); 
  if (type=="train") {assert_that(nrow(data)==length(trueClasses))}
  is.string(type); assert_that(type %in% c("train", "predict"))
  is.count(k); 
  assert_that(p %in% c(1, 2, Inf))
  if (type=="predict") {assert_that(not_empty(memory) & ncol(memory)==ncol(data) & nrow(memory)==length(trueClasses))}  
  numberobs <- nrow(data)
  if (type=="train") {
    predictionId <- 1
    distancematrix <- matrix(NA, numberobs, numberobs)
    for (n in 1:numberobs) {
      probe <- as.numeric(data[n,])
      probeExpanded <- matrix(rep(probe, each=numberobs), nrow=numberobs)
      if (p %in% c(1,2)) {distancematrix[n, ] <- (rowSums((abs(data - probeExpanded))^p) )^(1/p)}
      else 
        if (p==Inf) {distancematrix[n, ] <- apply(abs(data - probeExpanded), 1, max)}}
  } else 
  if (type == "predict") {
    predictionId <- 0
    noMemory <- nrow(memory)
    distancematrix <- matrix(NA, numberobs, noMemory)
    for (n in 1:numberobs) {
      probe <- as.numeric(data[n,])
      probeExpanded <- matrix(rep(probe, each=noMemory), nrow=noMemory)
      if (p %in% c(1,2)) {distancematrix[n, ] <- (rowSums((abs(memory - probeExpanded))^p) )^(1/p)} 
      else
        if (p==Inf) {distancematrix[n, ] <- apply(abs(memory - probeExpanded), 1, max)}}
  } 
  neighbours <- apply(distancematrix, 1, order)
  prob <- predictedClasses <-  rep(0, numberobs)
  for (n in 1:numberobs) {
    prob[n] <- Mode(trueClasses[neighbours[(1+predictionId):(k+predictionId), n]])
    if(prob[n]==1) {predictedClasses[n] <- 1} 
    else     
      if(prob[n]==2) {predictedClasses[n] <- 2} 
    else {predictedClasses[n] <- 3}
  }
  if (type=="train") {
    errorCount <- table(predictedClasses, trueClasses)
    accuracy <- mean(predictedClasses==trueClasses)
  } 
  else 
    if (type == "predict") {errorCount <- NA; accuracy <- NA}
  return(list(predictedClasses=predictedClasses, prob=prob, accuracy=accuracy,errorCount=errorCount))
}

#####################################################################################
