## Find the inverse of a matrix if it is invertible.

## The first function caches the matrix.

## set the matrix
## get the matrix
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) I <<- solve
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        

}


## This function determines if inverse of the matrix has already been calculated.
## If it has been calculated above, it gets the inverse from the function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        I <- x$getinverse()
        if(!is.null(I)) {
                message("retrieving cached matrix")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I        
}


