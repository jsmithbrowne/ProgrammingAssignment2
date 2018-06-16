##This function takes a matrix and uses solve to determine the inverse

## makeCacheMatrix takes a matrix and builds functions 
## these functions are returned via a list which makes the elements accessible by name

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    ##When set is called x is set the matrix and m, which will be our inverse is reset
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ##mutator methods
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a matrix and if the inverse matrix variable is NULL
## the inverse is calculated and assigned using the mutator methods in makeCashMatrix
## otherwise the accessor method of makeCashMatrix is used to return the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
