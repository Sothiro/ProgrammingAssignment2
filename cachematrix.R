## Rprogramming homework 3
## Sothiro pin

## makeCacheMatrix generates a matrix and stores its inverse if calculated using the
## cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## CacheSolve will return the inverse of the matrix x, using the cached value if available (m is non null)
## If not it will compute the inverse and store it using the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


## example with a random 5 by 5 matrix

x=matrix(runif(100,-10,10),5,5)
print("this is a random matrix:")
print(x)
z=makeCacheMatrix(x)
cacheSolve(z)
print("this is its inverse:")
print(z$getinv())