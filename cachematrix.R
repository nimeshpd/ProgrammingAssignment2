## Caching the invmatrixerse of a Matrix:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its invmatrixerse.
## which is really a list containing a function to
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinvmatrixerse <- function(invmatrixerse) invmatrix <<- invmatrixerse
  getinvmatrixerse <- function() invmatrix
  list(set = set,
       get = get,
       setinvmatrixerse = setinvmatrixerse,
       getinvmatrixerse = getinvmatrixerse)
}



## cacheSolve: 
## This function computes the invmatrixerse of the special "matrix" returned by makeCacheMatrix above. 
## If the invmatrixerse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the invmatrixerse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the invmatrixerse of 'x'
  invmatrix <- x$getinvmatrixerse()
  if (!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  mat <- x$get()
  invmatrix <- solve(mat, ...)
  x$setinvmatrixerse(invmatrix)
  invmatrix
}

# Output of My function
# > matrix <- makeCacheMatrix(matrix(2:5, 2, 2))
# > matrix$get()
#        [,1] [,2]
#  [1,]    2    4
#  [2,]    3    5
# > cacheSolve(matrix)
#       [,1] [,2]
#  [1,] -2.5    2
#  [2,]  1.5   -1

