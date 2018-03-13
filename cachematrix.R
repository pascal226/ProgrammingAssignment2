## Two functions that are able to create a product that caches the inverse of a matrix
## and resolves it.

## The first function is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
          m <- NULL
          set <- function (y) {
               x <<- y
               m <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse 
          getinverse <- function() m
          list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }


##The second function checks if the inverse has already been created, if so it gets it
##from the cache. If not it creates the inverse by itself and stores it in the cache.

cacheSolve <- function(x, ...) {
  
        m <- x$getinverse()
        if(!is.null(m))  {
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
  }
