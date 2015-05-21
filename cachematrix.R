## The functions below are designed to cache the inverse of a matrix


## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## the special matrix is really a list containing functions to:
##  1.Set the value of the matrix
##  2.Get the value of the matrix
##  3.Set the value of the inverse of the matrix
##  4.Get the value of the inverse of the matrix
##  assume that the matrix supplied is always invertible 
##  so we should not use the matrix which is not invertible, such as matrix(1:16,nrow=4,ncol=4),
##  to test the functions.

makeCacheMatrix <- function(x = matrix()) {
       i<- NULL   
       set <- function(y) {          ##set() :  initialise a new matrix
                x <<- y
                i <<- NULL
       }
       get <- function() x           ##get() :  get the matrix we put into the function
       setInverse <- function(Inverse) i <<- Inverse   
                                     ##setInverse() : set the inverse of the matrix, i, in the cache,
                                     ## be careful about this function 
       getInverse <- function() i    ##getInverse() : show us that the inverse of matrix has been stored 
       list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
       
}


## cacheSolve :Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
        ## first check to see if the inverse of a matrix has already been calculated
        i <- x$getInverse()         
        if(!is.null(i)) {
                message("getting cached data")
                return(i)   
        ## if so,  gets the mean from the cache and skips the computation
        }

        ## if not, calculates the inverse of the matrix and return the result
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        ##  sets the result in the cache via the setInverse function.    
        i
}
