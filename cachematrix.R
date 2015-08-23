## cache the time-consuming computations for inverse a matrix
## 

## makeCacheMatrix creates a special "vector" which really has a list 
## containing four function to
## 1. set the matrix
## 2. get the matrix
## 3. set the matrix of the inverse
## 4. get the matrix of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inv) inverse <<- inv
        
        getinverse <- function() inverse
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates the inverse of matrix which created with the above function.
## it first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse of matrix in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting inverse matrix from cache data")
                return(inverse)
        }
        data <- x$get()
        
        ## if data is not a square matrix
        if ( dim(data)[1] != dim(data)[2] ){
                message("matrix must be square")
                return(NULL)
        }
                
        inverse <- solve(data, ...)

        x$setinverse(inverse)
        inverse
}