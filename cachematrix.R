## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##There are two functions makeCacheMatrix and cashesolve
## here first we will the set the value
## of the matrix and get the value of the matrix,in the other we will set the 
## value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){     
      ##setting the value of matrix using another function
      x <<- y
      inv <<- NULL         
    }
    get <- function() {x}         ##this function gets the matrix
    setInverse <- function(inverse) {inv <<-inverse}
    getInverse <- function(){inv}
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)  ##this function obtain the inverse of matrix
  

}


## Write a short comment describing this function
## This function is used to get the cache data 

cacheSolve <- function(x, ...) {
                           ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()    ##gets inverse of the matrix and assigns to inv
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## this function does the calculate of the inverse of the matrix 
  mat <- x$get()
  inv <- solve(mat, ...)    ##calculates inverse values
  x$setInverse(inv)         ##To checks if the inverse is null
  inv                       ## returns the inverse value
}
