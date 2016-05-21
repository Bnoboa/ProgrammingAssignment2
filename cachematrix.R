## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache
makeCacheMatrix <- function(x = matrix()) {
        invrt <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function (inverse) invrt <<- inverse
        getInv <- function() invrt
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message ("getting cached data")
                return (invrt)
        }
        mat.data <- x$get()
        invrt <- solve(mat.data, ...)
        x$setInv(invrt)
        return(invrt)
        ## Return a matrix that is the inverse of 'x'
}

