## Put comments here that give an overall description of what your
## functions do
#The first fuction create a special matrix that can put in cache its inverse, the second
#compute this special matrix, and if this is already calculated and dont change, the cahce 
#solve retrieve the inverse from the cache.

## Write a short comment describing this function

#this first fuction calculate de invers matrix and save in cache the result.

makeCacheMatrix <- function(x = matrix()) {
  invM<-NULL
  setM<-function(y){
    x<<-y
    invM<<-NULL
  }
  
  getM<-function()x
  setInv<-function(inverse) invM<<-inverse
  getInv<-function() invM
  list(setM=setM,getM=getM,setInv=setInv,getInv=getInv)
  

}

## Write a short comment describing this function

#This second fuction looks if the solved matrix is in cache. If it's in cache return
##the existent matrix with out calcul this, if not, solve the new matrix and retur it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getInv()
  if(!is.null(invM)) {
    message("This matrix is in cache")
    return(invM)
  }
  dataM <- x$getM()
  invM <- solve(dataM, ...)
  x$setInv(invM)
  invM
}
