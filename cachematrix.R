## There are two functions that are used to create a special object 
## that stores a square matrix and cache's its inverse. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## 
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached solve")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setsolve(s)
        s
}
