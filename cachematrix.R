###############################################################################
## The functions in this script are aimed to avoid unnecessary computation 
## of the inverse of a given matrix. Take as example the naive example 
## described below,
## 
## x <- matrix(rnorm(16),4,4) 
## for (i in 1:10){
##   ix <- solve(x)  # The inverse is computed 10 times
## }
## 
## However, one could use makeCacheMatrix and cacheSolve functions present 
## in this script. The first introduce a new class of object that can store 
## the matrix's inverse, and the former checks if the the inverse is available; 
## if yes, return the cached inverse matrix; if not, the inverse is calculated. 
## The example presented above can be replaced by,
## 
## x <- matrix(rnorm(16),4,4) 
## M <- makeCacheMatrix(x)
## for (i in 1:10){
##    I <- cacheSolve (M) # The inverse matrix is computed only for i equal to 1, 
##                        # all other interactions the cached inverse matrix is 
##                        # returned.
## } 
###############################################################################


#   makeCacheMatrix <- function(x = numeric()) creates a special "matrix" object 
#   that can cache its inverse.
#
#   Use: M <- makeCacheMatrix(x)
#        where x is a square and invertible.  
#  
#   Commands:
#   set the value of the matrix,  Ex: M$set(y), where y is an invertible matrix.
#   get the value of the matrix,  Ex: m <- M$get()
#   set the value of the inverse, Ex: M$setInv(m)
#   get the value of the inverse, Ex: i <-M$getInv() 
makeCacheMatrix <- function(x = numeric()) {
  InvM <- NULL                             # Inverse matrix equal to NULL
  set <- function(y) {
    x <<- y                                # Set new value for matrix
    InvM <<- NULL                          # Set inverse matrix equal to NULL
  }
  get    <- function() x                   # Return matrix values
  setInv <- function(solve) InvM <<- solve # Evaluate the inverse matrix
  getInv <- function() InvM                # Return the inverse matrix
  list(set = set,                          # Generate the object list
       get = get,
       setInv = setInv,
       getInv = getInv)
}


#   cacheSolve <- function(x, ...) computes the inverse of the special "matrix" 
#   returned by makeCacheMatrix. 
#  
#   Use: I <- cacheSolve(M)
#        where M is obtained using makeCacheMatrix function.
cacheSolve <- function(x, ...) {  
  m <- x$getInv()          # Check if inverse matrix is available.
  if(!is.null(m)) {        # If yes, return cached inverse to avoid calculation.
    message("Getting cached inverse matrix")
    return(m)
  }
  data <- x$get()          # Recover matrix
  m    <- solve(data, ...) # Get its inverse
  x$setInv(m)              # Store it
  m                        # Return
}
