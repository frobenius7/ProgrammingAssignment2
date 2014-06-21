## We have created a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "vector", which 
## is really a list containing a function to
## 1)set the value of the matrix
## 2)get the value of the matrix
## 3)set the value of the inverse matrix
## 4)get the value of the inverse matrix

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

## The 2nd function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## check if we have cached version of result
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if not found in cache - calculate the inverse 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        
}

#Testing examples:
#y<-makeCacheMatrix(matrix(c(1,2,3,4),2,2))
#cacheSolve(y)
#y$set(matrix(c(1,2,2,6),2,2))
#cacheSolve(y)