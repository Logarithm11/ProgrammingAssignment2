## These functions are used to cache inverses of matrices to 
## avoid unnecessary commputations as finding the inverse of
## a matrix can be very time consuming. 

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        B = NULL
        set = function(C){
                x <<- C
                B <<- NULL
        }
        get = function() x
        setInverse = function(inverse) B <<- inverse
        getInverse = function() B
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)

}


## Computes the inverse of a matrix created by 
## "makeCacheMatrix". If the inverse has already been cached,
## this function will simply return the cached value. 

cacheSolve <- function(x, ...) {
        B = x$getInverse()
        if (!is.null(B)){
                message("getting cached data")
                return(B)
        }
        data = x$get()
        B = solve(data, ...)
        x$setInverse(B)
        B
}
