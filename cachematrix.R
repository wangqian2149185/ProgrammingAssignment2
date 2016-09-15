## The makeCacheMatrix function creates 4 special functions, 
## preparation for 2nd function to cache or calculate 
## the inverse of a supplied matrix.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## get the result preserved in cache if the solve function has bee used before on same matrix
## if not, calculate it and save the result to cache, then next time it can be retrieved directly from cache

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- inverse(data, ...)
     x$setinverse(m)
     m
}

 my_matrix<-matrix(c(1,-1,1,2),2,2)              # an example, a invertible matrix my_traix
 makeCacheMatrix(my_matrix)->my_intermediate     # 1st function to creat list for 2nd one
 cacheSolve(my_intermediate)->my_inverse         # plug in to 2nd function cacheSolve() to get final inverse matrix
 my_inverse                                      # print the inverse matrix
