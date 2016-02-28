## Matrix inversion is often a costly computation and 
## it may be advantageous to cache the inverse of a
## matrix as opposed to repeatedly computing it. The
## following functions allow the inverse of a matrix
## to be cached and retrieved.

## It is assumed that any supplied matrix is
## invertible.

## The makeCacheMatrix function creates a special
## "matrix" object which can cache a matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invmat <<- inverse
  getinv <- function() invmat
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve computes the inverse of a matrix
## that is returned by the makeCacheMatrix function.
## If the inverse has already been computed and there
## have been no changes to the matrix, the inverse is
## retrieved from the cache.

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("Acquiring Cached Data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}



## Testing the functions
## Test 1
testmatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
testmatrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
testmatrix$getinv()
# NULL
cacheSolve(testmatrix)
#[,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
cacheSolve(testmatrix)
# Acquiring Cached Data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
testmatrix$getinv()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


## Test 2
testmatrix2 <- makeCacheMatrix(matrix(c(2, 10, 9, 5), 2, 2))
testmatrix2$get()
# [,1] [,2]
# [1,]    2    9
# [2,]   10    5
testmatrix2$getinv()
# NULL
cacheSolve(testmatrix2)
# [,1]    [,2]
# [1,] -0.0625  0.1125
# [2,]  0.1250 -0.0250
cacheSolve(testmatrix2)
# Acquiring Cached Data
# [,1]    [,2]
# [1,] -0.0625  0.1125
# [2,]  0.1250 -0.0250
testmatrix2$getinv()
# [,1]    [,2]
# [1,] -0.0625  0.1125
# [2,]  0.1250 -0.0250
