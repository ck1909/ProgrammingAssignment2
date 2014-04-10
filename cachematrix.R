#the below functions together implement a cacheable matrix inverse

#function makeCacheMatrix
#takes invertible matrix as input
#keeps a copy of (and allows to get/set) the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #sets copy of matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #gets copy of matrix
    get <- function() x
    #sets copy of inverse
    setinv <- function(i) inv <<- i
    #gets copy of inverse
    getinv <- function() inv
    #maintains matrix and its inverse in a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


#function cacheSolve
#computes the inverse of matrix object returned by makeCacheMatrix
#if the inverse has already been calculated (and matrix object has not changed),
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    #get potentially cached inverse
    inv <- x$getinv()
    if(!is.null(inv)) {
        #if inverse is cached, return it
        message("getting cached data")
        return(inv)
    }
    #otherwise, compute and cache inverse for later use
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    #return inverse at the end
    inv
}

#Sample test case:
#m <- matrix(2:5, 2, 2)
#mc <- makeCacheMatrix(m)
#now when cacheSolve(mc) is run multiple times,
#the cached inverse will be used in from second run onwards,
#and message "getting cached data" will be outputted