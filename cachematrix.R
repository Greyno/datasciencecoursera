## For a potentially time-consuming calculation, these functions will first find the inverse of a matrix and then cache the solution. If the inverse has already been calcuated, the cached solution will be returned. 
#This program was forked from Github using Terminal via ->github clone https:<web location in my Github repo>

## The first function, makeCacheMatrix creates a special "vector", which is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve functions returns the inverse of the matrix. It first checks if the inverse has already been caluclated. If the inverse has already been calculated, it returns the cached solution and skips computation. Otherwise, the function calculates the inverse and sets the value of the inverse in the cache via the setinverse function. The assumption for the inverse calculation is that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {    #Solution already exists, get cached data
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)  #Find the inverse if not cached
    x$setinverse(inv)
    inv
}

