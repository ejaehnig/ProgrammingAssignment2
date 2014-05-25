## Functions determine if matrix has already been inverted (saved in MatrixCache);
## If not, matrix is inverted using the solve function and saved in MatrixCache;
## If so, matrix inversion is skipped and MatrixCache is returned instead.

## makeCacheMatrix is used to set and get MatrixCache, 
## which stores an inverted matrix as the cache, or is NULL if the cache has not been set

makeCacheMatrix <- function(x = matrix()) {
     MatrixCache <- NULL
     set <- function (y){
          x <<- y
          MatrixCache <<- NULL
     }
     get <- function() x
     setmatrix <- function(InvMatrix) MatrixCache <- InvMatrix
     getmatrix <- function() MatrixCache
     list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## cacheSolve calls makeCacheMatrix's getmatrix subfunction to check if MatrixCache has been set
## if it has, MatrixCache is returned; if not, the matrix is inverted using the solve function
## and makeCacheMatrix's setmatrix subfunction is used to set MatrixCache

cacheSolve <- function(x, ...) {
     MatrixCache <- x$getmatrix()
     if(!is.null(MatrixCache)){
          message("getting cached data")
          return(MatrixCache)
     }
     data <- x$get()
     MatrixCache <- solve(data, ...)
     x$setmatrix(MatrixCache)
     MatrixCache
}
