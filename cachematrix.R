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


## Write a short comment describing this function

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