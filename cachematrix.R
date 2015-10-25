
## Function makeCacheMatrix creates a new environment that stores an initial matrix, invariant matrix and provides functions to
## set and get these matrices 

makeCacheMatrix <- function(x = matrix()) {
    originalMatrix <- x 
    invMatrix <- NULL
    
    ## set a new initial matrix 
    set <- function(y) {
        originalMatrix <<- y
        invMatrix <<- NULL
    }

    ## get an initial matrix 
    get <- function() {
        originalMatrix
    }

    ## set an inverted matrix 
    setInvMatrix <- function(sMatrix) {
        invMatrix <<- sMatrix 
    }    

    ## get an inverted matrix 
    getInvMatrix <- function() {
        invMatrix
    }
    
    list( set = set, get = get, 
          setInvMatrix = setInvMatrix, 
          getInvMatrix = getInvMatrix)
} 
 

## This function returns the cached value of inverted matrix or computes new invertef matrix  
    
cacheSolve <- function(x, ...) {

    ## attempt to get cached value for an inveted matrix
    invMatrix <- x$getInvMatrix()
     if (!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    
    ## if no cached inverted matrix then compute a new one 
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setInvMatrix(invMatrix)
    invMatrix
} 