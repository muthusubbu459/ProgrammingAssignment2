## Caching the inverse of a matrix
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly
## The functions below are used to cache the inverse of a matrix

## makeCacheMatrix creates a special matrix object which is really a list containing a function to:
## 1.Set the value of the matrix
## 2.Get the value of the matrix
## 3.Set the value of the inverse of the matrix
## 4.Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse        
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                          
}



## cacheSolve function calculates the inverse of the special matrix created with the above function. 
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the value and skips the calculation.
## Else, it calculates the inverse of the data using solve function and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
