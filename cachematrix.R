## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## Function cacheSolve computes the inverse of the special "matrix". If
## inverse already exists and matrix has not changed, then retrieve the inverse from cache.


## Create a special "matrix" and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}


## calculate the inverse of the matrix if not already exists, otherwise retrieve from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
    
}




