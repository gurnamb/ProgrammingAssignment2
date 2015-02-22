# Assignment: Caching the Inverse of a Matrix
# 
# Matrix inversion is usually a costly computation and there 
# may be some benefit to caching the inverse of a matrix rather 
# than compute it repeatedly (there are also alternatives to 
# matrix inversion that we will not discuss here). Your assignment
# is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#   1.makeCacheMatrix: This function creates a special "matrix" 
#     object that can cache its inverse.
#   2.cacheSolve: This function computes the inverse of the special 
#     "matrix" returned by makeCacheMatrix above. If the inverse has 
#     already been calculated (and the matrix has not changed), then 
#     the cachesolve should retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the 
# solve function in R. For example, if X is a square invertible 
# matrix, then solve(X) returns its inverse.

# 
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse
# 4.get the value of the inverse
# 
# setwd("J:/DATA_DRIVE/coursera/R_Programming/week3/programAssignment2")

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

# The following function calculates the inverse of the special "matrix" 
# created with the above function. However, it first checks to see 
# if the matrix has already been calculated. If so, it gets the matrix
# from the cache and skips the computation. Otherwise, it calculates 
# the matrix of the data and sets the value of the matrix in the cache 
# via the setInverse function.
# Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


#Testing
x <- matrix(1:4, nrow = 2, ncol=2)
m <- makeCacheMatrix(x)
m$get()
cacheSolve(m)
cacheSolve(m)

##  > #Testing
##  > x <- matrix(1:4, nrow = 2, ncol=2)
##  > m <- makeCacheMatrix(x)
##  > m$get()
##       [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
##  > cacheSolve(m)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > cacheSolve(m)
##  getting cached data
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > 

