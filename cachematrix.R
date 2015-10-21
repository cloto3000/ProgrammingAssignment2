## This two functions create and use stored inverted matrix with the 
## solve function 

## This function create a cache for a matrix and create all the internal 
## methods for the other function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y    ##store in x the original matrix
                m <<- NULL
        }
        ## get return the original matrix
        get <- function() x      
        ## calc the inverse matrix in m
        setsolve <- function(solve) m <<- solve   
        #return inversed matrix
        getsolve <- function() m 
        # create a list for naming functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve control if the inversed matrix was cached. If yes return cached 
## if no calc inversed matrix with solve and store in cache the result
cacheSolve <- function(x, ...) {
        m <- x$getsolve() ## get the inversed matrix and control if cached
        if(!is.null(m)) {
                message("getting cached data")
                return(m) ## return cached matrix
        }
        data <- x$get() ##else get the original matrix and solve it
        m <- solve(data, ...)
        x$setsolve(m) ## store the inversed matrix in m (cache)
        m
        ## Return a matrix that is the inverse of 'x'
}
