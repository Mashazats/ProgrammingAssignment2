## The two following functions creates an object matrix that can cach its inverse matrix, and the second function 


makeCacheMatrix <- function(x = matrix()) {     ##This function creates a special "matrix" object that can cache its inverse named inverse_matrix.

        inverse_matrix <- NULL
        set <- function(y) {
            x <<- y
            inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    
}


cacheSolve <- function(x, ...) {            ## Return a matrix that is the inverse of 'x' - first check out the cached inverse_matrix, if none than calculates the inverse
      
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
            message("getting cached inverse matrix")
            return(inverse_matrix)
         }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}
