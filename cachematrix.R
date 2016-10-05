## Put comments here that give an overall description of what your
## functions do
## The functions will create a maxtrix that is able to cache its inverse
## and retrieve the matrix from cache. If the inverse of the matrix has already
## been solved it will be retrieved from cache.
## We're assuming the input matrix is a square matirx

## Write a short comment describing this function
## This fuction will create a matrix that is capable of caching its inverse. The
## matrix is stored in the object x. We then initialize the inv object as NULL
## to hold the inverse of the matrix The portion of the code defines the
## behaviours of for objects created with the function. The set function
## is a creator function that pulls from the parent environment values for x,
## and inv. If a value for inv already exists it cleared from memory.
## The get function gets the value of the matrix, x from the parent environment.
## The setinv function defines the solve function for inv and the getinv 
## function returns the value for inv
## Lastly the function returns a list object containing functions to set
## value of the matrix, get the value matrix, set the value of the inverse
## and get the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## This function will compute the inverse of of a cached matrix created by 
## makeCacheMatrix. If the matrix inverse has been calculated it will retrieve
## the solved matrix from cache
## the function first retrieves the inverse object from the cached matrix object
## and then tests to see if the object is null. If the object is not null 
## the function displays a message and returns the already calculated inverse
## and exits. If the inverse object is null the function retrieves the
## original matrix from the cached matrix object, passes the matrix to the solve
## function and stores the resulting inversed matrix in the matrix object. It 
## returns the inverse matrix for display

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) { 
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
