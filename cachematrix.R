## Cache the inverse of a matrix and use that that to simplify a potentially time consuming computation

## This function (parent) creates a special matrix object that can cache it's inverse
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


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#If the inverse has already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting the cached inverse matrix")
          return(m)
        }
        data <- x$get()
       
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##  To test the function :
#x<-matrix(1:4,2,2)
#mat<- makeCacheMatrix(x)
#cacheSolve(mat)  ## calculates the first time
#cacheSolve(mat)  ## gets it from cache the next time

