## Put comments here that give an overall description of what your
## functions do

## Creating a matrix object that is supposed to be an invertible matrix with four nested fuctions 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y 
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Retrieving the inverse of the matrix identified by MakeCachematrix; if there is no cached value in the object
## calculating and caching the inverse of the matrix 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
data <- x$get()
inverse <- solve(data, ...)
x$setinverse(inverse)
inverse
}

### testing the code 
matrix1 = makeCacheMatrix(matrix(rnorm(9, mean = 6, sd = 2), nrow=3))
cacheSolve(matrix1)
matrix1$get()
matrix1$getinverse()
matrix1$set(matrix(1:9, nrow=3))  
cacheSolve(matrix1) ## singular matrix = no inverse is possible 
