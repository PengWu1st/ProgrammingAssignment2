## a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y){
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinvs <- function(inverse) invs <<- inverse
        getinvs <- function() invs
        list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvs()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvs(m)
        m
        
}
